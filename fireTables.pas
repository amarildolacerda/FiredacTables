unit fireTables;

interface


{$I Delphi.inc}
{$IFDEF ANDROID}
{$DEFINE BDE2FIREDAC}
{$DEFINE FIREDAC}
{$ENDIF}
{.$DEFINE USACUSTOMDLG}
{$IFDEF WINDBU }
{$DEFINE LOCALSQL}
{$ENDIF }
{$IFDEF FIREDAC}
{$DEFINE LOG}
{$IFNDEF MSWINDOWS}
{$UNDEF USACUSTOMDLG}
{$UNDEF LOG}
{$ENDIF}
{$IFDEF CPUX64}
{$DEFINE BDE2FIREDAC}
{$ENDIF}

{$IFDEF BPL}
 {$UNDEF USACUSTOMDLG}
 {$UNDEF LOG}
 {$UNDEF VCL}
 {$UNDEF FMX}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
  FireDAC.Comp.DataMove,
  FireDAC.Comp.ScriptCommands,
  FireDAC.Comp.Script,
  Winapi.Messages,
{$ENDIF}
  Data.DB, FireDAC.Stan.Param, FireDAC.Stan.Intf, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Stan.Option,
  FireDAC.DApt.Intf,
  System.SysUtils, System.Variants,
  System.Classes,
{$IFDEF FMX}
  FMX.Graphics,
{$ELSE}
  Vcl.Graphics,
{$ENDIF}
  IniFiles;   // {, stConstantes} - AL nao pode ter codigo interno - Compatibilidade com Android

const // AL - nao pode mudar este codigo de lugar
  c_alias_Local_SQLite = 'LOCAL_SQLITE'; // nao alterar

type

  TFireTransIsolation = (tiDirtyRead, tiReadCommitted, tiRepeatableRead);

  TFireDatabase = class;

  TFireTableType = (ttDefault, ttParadox, ttDBase, ttFoxPro, ttASCII);
{$IFDEF FIREDAC}
  TTableType = TFireTableType;
  TTransIsolation = TFireTransIsolation;
{$ENDIF}
  TFireDatabaseEvent = (dbOpen, dbClose, dbAdd, dbRemove, dbAddAlias, dbDeleteAlias, dbAddDriver, dbDeleteDriver);

  TFireDatabaseNotifyEvent = procedure(DBEvent: TFireDatabaseEvent; const Param) of object;

  TFireSession = class;

  TFireDatabase = class(TFDCustomConnection)
  private
    FAliasName: string;
    FKeepConnection: Boolean;
    FSessionName: String;
    FSession: TFireSession;
    FTransIsolation: TFireTransIsolation;
    FDriveType: string;
    procedure SetDatabasename(const Value: string);
    function GetDatabasename: string;
    procedure SetAliasName(const Value: string);
    procedure SetKeepConnection(const Value: Boolean);
    procedure SetSessionName(const Value: String);
    procedure SetSession(const Value: TFireSession);
    procedure SetTransIsolation(const Value: TFireTransIsolation);
{$IFDEF USACUSTOMDLG}
    procedure DoLoginDialogEvent(AConnection: TFDCustomConnection; const AConnectionDef: IFDStanConnectionDef); {$ENDIF}
    procedure DoAfterConnectEvent(sender: TObject);
    procedure DoConnect; override;
  public
    Constructor Create(ow: TComponent); override;
    Destructor Destroy; override;
    procedure CloseDataSets;
    property KeepConnection: Boolean read FKeepConnection write SetKeepConnection;
    property Session: TFireSession read FSession write SetSession;
    procedure GetColumnNames(ATable: string; aTmp: string; ALst: TStrings);
    procedure GetFieldNames(tabela: string; lst: TStrings); overload;
    procedure GetTableNames(lst: TStrings; sysObjs: Boolean); overload;
  published

    Property AliasName: string read FAliasName write SetAliasName;
    Property Databasename: string read GetDatabasename write SetDatabasename;
    property SessionName: String read FSessionName write SetSessionName;
    property TransIsolation: TFireTransIsolation read FTransIsolation write SetTransIsolation;
    property LoginPrompt;
  end;

  TFireUpdateSql = class;

  TFireQuery = class(TFDCustomQuery)
  private
    FSQL: TStrings;
    FDatabasename: string;
    FSessionName: String;
    FParamCheck: Boolean;
    FAutoRefresh: Boolean;
    FOnUpdateError: TFDUpdateErrorEvent;
    FOnReconcileError: TFDReconcileErrorEvent;

    function GetDatabase: TFireDatabase;
    procedure SetDatabase(const Value: TFireDatabase);
    procedure SetDatabasename(const Value: string);
    function GetRequestLive: Boolean;
    procedure SetRequestLive(const Value: Boolean);
    procedure SetUniDirectional(const Value: Boolean);
    procedure SetUpdateMode(const Value: TUpdateMode);
    function GetUpdateMode: TUpdateMode;
    procedure SetSessionName(const Value: String);
    procedure SetParamCheck(const Value: Boolean);
    function GetUpdateObject: TFireUpdateSql;
    procedure SetUpdateObject(const Value: TFireUpdateSql);
    procedure SetAutoRefresh(const Value: Boolean);
    function GetDataSource: TDataSource; override;
    procedure SetDatasource(const Value: TDataSource);
    procedure InitNullParams;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure SqlChanged(sender: TObject);
    function GetUniDirectional: Boolean;
    function GetChacedUpdates: Boolean;
    procedure SetCachedUpdates(const Value: Boolean);
    procedure SetOnUpdateError(const Value: TFDUpdateErrorEvent);
  protected
    FErrorMessage: string;
    FErrorCount: Integer;
    procedure InternalClose; override;
    procedure InternalPost; override;
    procedure SetActive(AValue: Boolean); override;
    procedure DoUpdateError(ASender: TDataSet; AException: EFDException; ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction);
    procedure DoReconcileError(DataSet: TFDDataSet; E: EFDException; UpdateKind: TFDDatSRowState; var Action: TFDDAptReconcileAction);

  public
    useLocalSql:boolean;
    procedure FreeBookMark(book: TBookMark); override;
    function GetBookMark: TBookMark; override;
    procedure GotoBookmark(book: TBookMark); reintroduce; virtual;
    function GetAfterInsert:TDataSetNotifyEvent;
    procedure SetAfterInsert(value:TDataSetNotifyEvent);

    function BookmarkValid(Bookmark: TBookMark): Boolean; override;
    property FieldOptions;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    property Database: TFireDatabase read GetDatabase write SetDatabase;
    function RowsAffected: Integer;
    procedure Prepare;
    procedure InternalOpen; override;
    Procedure ExecDirect;
    procedure Execute(ATimes: Integer = 0; AOffset: Integer = 0); override;

  published
    property Active;
    Property AutoCalcFields;
    property OnUpdateError: TFDUpdateErrorEvent read FOnUpdateError write SetOnUpdateError;
    property OnReconcileError: TFDReconcileErrorEvent read FOnReconcileError write FOnReconcileError;
    property CachedUpdates: Boolean read GetChacedUpdates write SetCachedUpdates default false;
    property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh default false;
    property Databasename: string read FDatabasename write SetDatabasename;
    property DataSource: TDataSource read GetDataSource write SetDatasource;
    property RequestLive: Boolean read GetRequestLive Write SetRequestLive default true;

    property SQL: TStrings read GetSQL write SetSQL;
    property UniDirectional: Boolean read GetUniDirectional write SetUniDirectional default false;
    property UpdateMode: TUpdateMode read GetUpdateMode write SetUpdateMode;

    Property Params;

    property UpdateObject: TFireUpdateSql read GetUpdateObject write SetUpdateObject;
    property SessionName: String read FSessionName write SetSessionName;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck default true;
    // : boolean read FCachedUpdated write SetCachedUpdated;
    property BeforeEdit;
    property AfterEdit;
    property AfterCancel;
    property AfterScroll;
    property AfterPost;
    property AfterInsert :TDataSetNotifyEvent read GetAfterInsert write SetAfterInsert;
    property AfterDelete;
    property BeforeCancel;
    property BeforePost;
    property BeforeDelete;
    property BeforeInsert;
    property OnCalcFields;
    property OnPostError;
    // property OnUpdateError;
    property OnDeleteError;
    property BeforeScroll;
    property Filtered;
    property Filter;
    property OnFilterRecord;
  end;

  TFireScripts = class(TFDScript)
  private
    FDatabasename: String;
    function GetDatabasename: string;
    procedure SetDatabasename(const Value: string);
    function GetDatabase: TFireDatabase;
    procedure SetDatabase(const Value: TFireDatabase);
  public
    property Database: TFireDatabase read GetDatabase write SetDatabase;
    Property Databasename: string read GetDatabasename Write SetDatabasename;
    function ExecuteAll: Boolean; overload;
    procedure Clear;

  end;

  TFireScript = class(TFireScripts)
  private
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
  public
    procedure ExecSQL;
  published
    property SQL: TStrings read GetSQL write SetSQL;
  end;

  TFireParam = TFDParam;

  { TFireParamHelper = class helper for TFireParam
    private
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
    public
    property Value:Variant read GetValue write SetValue;
    end;
  }

  TFireParams = TFDParams;

  TFireStoredProc = class(TFDCustomStoredProc)
  private
    FDatabasename: String;
    FSessionName: string;
    procedure SetDatabasename(const Value: String);
    function GetDatabase: TFireDatabase;
    procedure SetDatabase(const Value: TFireDatabase);
    procedure SetSessionName(const Value: string);
  protected
  public
    function ExecSQL: Integer;
    property Database: TFireDatabase read GetDatabase Write SetDatabase;
  published
    property Params;
    property Databasename: String read FDatabasename write SetDatabasename;
    property SessionName: string read FSessionName write SetSessionName;

    property CatalogName; // : String read GetCatalogName write SetCatalogName;
    property SchemaName; // : String read GetSchemaName write SetSchemaName;
    property PackageName; // : String read GetPackageName write SetPackageName;
    property StoredProcName; // : string read GetProcName write SetProcName;

    property Active;
    property AutoCalcFields;
    property FilterOptions;
    property Filter;
    property OnFilterRecord;
    property UpdateObject;
  end;

  TFireTable = class(TFDTable)
  private
    FDatabasename: String;
    FSessionName: string;
    FReadOnly: Boolean;
    FTableType: TFireTableType;
    FUpdateMode: TUpdateMode;
    FDefaultIndex: Boolean;
    procedure SetDatabasename(const Value: String);
    function GetDatabase: TFireDatabase;
    procedure SetDatabase(const Value: TFireDatabase);
    procedure SetSessionName(const Value: string);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetTableType(const Value: TFireTableType);
    procedure SetActive(AValue: Boolean); override;
    function GetTableName: string;
    procedure SetTableName(const Value: string);
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetDefaultIndex(const Value: Boolean);
  public
    property Database: TFireDatabase read GetDatabase write SetDatabase;
    procedure CreateTable;
    procedure EmptyTable;
    procedure DeleteTable;
  published
    property DefaultIndex: Boolean read FDefaultIndex write SetDefaultIndex;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default false;
    property Databasename: String read FDatabasename write SetDatabasename;
    property SessionName: string read FSessionName write SetSessionName;
    property TableType: TFireTableType read FTableType write SetTableType default ttDefault;
    property TableName: string read GetTableName write SetTableName;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereAll;

  end;

  TFireSession = class(TComponent)
  private
    FConfigFile: TMemIniFile;
    FOnDbNotify: TFireDatabaseNotifyEvent;
    FSessionName: String;
    FActive: Boolean;
    FnetFileDir: String;
    procedure SetOnDbNotify(const Value: TFireDatabaseNotifyEvent);
    function GetDatabases(i: Integer): TFireDatabase;
    procedure SetDatabases(i: Integer; const Value: TFireDatabase);
    procedure SetSessionName(const Value: String);
    procedure SetActive(const Value: Boolean);
    procedure SetnetFileDir(const Value: String);
  public
    constructor Create(ow: TComponent); override;
    destructor Destroy; override;
    function IsAlias(sAlias: String): Boolean;
    procedure AddDriver(const Name: string; List: TStrings);
    function GetAliasDriverName(sAlias: String): String;
    procedure GetAliasParams(sAlias: String; str: TStrings);
    procedure AddStandardAlias(alias, path, driver: string);
    procedure SaveConfigFile;
    property netFileDir: String read FnetFileDir write SetnetFileDir;
    procedure ModifyAlias(alias: String; ts: TStrings);
    function FindDataBase(alias: String): TFireDatabase;
    property OnDbNotify: TFireDatabaseNotifyEvent read FOnDbNotify write SetOnDbNotify;
    function DatabaseCount: Integer;
    procedure Close;
    procedure Open;
    property Databases[i: Integer]: TFireDatabase read GetDatabases write SetDatabases;
    procedure CloseDatabases;
    function OpenDatabase(dBase: string): TFireDatabase;
    procedure CloseDatabase(lDb: TFireDatabase);
    procedure DeleteAlias(sAlias: string);
    procedure AddAlias(sAlias: string; driver: string; List: TStrings);
    procedure GetTableNames(DBName, msk: String; a, b: Boolean; Dest: TStrings);
    procedure GetAliasNames(items: TStrings);
    procedure GetDatabaseNames(L: TStrings);
  published
    property SessionName: String read FSessionName write SetSessionName;
    property Active: Boolean read FActive write SetActive;
  end;

  TFireSessions = class(TComponent)
  private
    FAliases: TStringList;
    function GetItems(idx: Integer): TFireSession;
    procedure SetItems(idx: Integer; const Value: TFireSession);
    function GetList(sSession: string): TFireSession;
    procedure SetList(sSession: string; const Value: TFireSession);
    procedure SetCurrentSession(const Value: TFireSession);
    function GetCurrentSession: TFireSession;
  public
    procedure LoadConfig;
    function IsAlias(sAlias: String): Boolean;
    constructor Create(own: TComponent); override;
    destructor Destroy; override;
    function count: Integer;
    function FindSession(sSession: String): TFireSession;
    procedure OpenSession(sSession: String);
    property Sessions[idx: Integer]: TFireSession read GetItems write SetItems; default;
    property List[sSession: string]: TFireSession read GetList write SetList;
    property CurrentSession: TFireSession read GetCurrentSession write SetCurrentSession;
  end;

  TFireUpdateSql = class(TFDUpdateSql)
  private
    FQuery: array [ukModify .. ukDelete] of TFireQuery;
    procedure SetDatabasename(const Value: String);
    function GetQuery(kd: TUpdatekind): TDataSet;
    // procedure SetQuery(kd: TUpdatekind; const Value: TDataset);
    function GetDatabasename: String;
    function GetDataset: TDataSet;
    procedure SetDataset(const Value: TDataSet);
    function GetDeleteSQL: TStrings;
    procedure SetDeleteSQL(const Value: TStrings);
    function GetInsertSQL: TStrings;
    procedure SetInsertSQL(const Value: TStrings);
    function GetModifySQL: TStrings;
    procedure SetModifySQL(const Value: TStrings);

  protected
    function GetSQL(UpdateKind: TUpdatekind): TStrings; virtual;
  public
    procedure SetParams(ADataset: TDataSet; UpdateKind: TUpdatekind); overload; virtual;
    procedure SetParams(UpdateKind: TUpdatekind); overload; virtual;
  public
    constructor Create(ow: TComponent); override;
    destructor Destroy; override;
    property DataSet: TDataSet read GetDataset write SetDataset;
    property Query[kd: TUpdatekind]: TDataSet read GetQuery; // write SetQuery;
    procedure ExecSQL(UpdateKind: TUpdatekind); virtual;
    procedure Apply(UpdateKind: TUpdatekind); virtual;
    property Databasename: String read GetDatabasename write SetDatabasename;
  published
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
  end;

{$IFDEF MSWINDOWS}

  TFireBatchMode = (batAppend, batUpdate, batAppendUpdate, batDelete, batCopy);

  TFireBatchMove = class(TFDDataMove)
  private
    FMode: TFireBatchMode;
    FRecordCount: Integer;
    FTableType: TFireTableType;
    procedure SetMode(const Value: TFireBatchMode);
    function GetDestination: TFireTable;
    function GetSource: TDataSet;
    procedure SetDestination(const Value: TFireTable);
    procedure SetSource(const Value: TDataSet);
    procedure SetRecordCount(const Value: Integer);
    procedure SetTableType(const Value: TFireTableType);
  public
    property RecordCount: Integer read FRecordCount write SetRecordCount;
    procedure Execute;
    property TableType: TFireTableType read FTableType write SetTableType;
  published
    property Source: TDataSet read GetSource write SetSource;
    property Destination: TFireTable read GetDestination write SetDestination;
    property Mode: TFireBatchMode read FMode write SetMode;

  end;
{$ENDIF}
{$IFDEF VER270}
{$IFDEF BDE2FIREDAC}

  TLocale = class(TObject);
  TUpdateSql = class(TFireUpdateSql);
  TDatabase = class(TFireDatabase);
  TQuery = class(TFireQuery);
  TTable = class(TFireTable);
  TDBDataSet = class(TFireQuery);
  TBDEDataset = class(TFireQuery);
  TSession = class(TFireSession);
  TStoredProc = class(TFireStoredProc);
  TBatchMove = class(TFireBatchMove);

  TSessions = class(TFireSessions)
  public
    function FindSession(sSession: String): TSession;
  end;



function Session: TSession;
function Sessions: TSessions;
{$ENDIF}
{$ENDIF}
function FireSession: TFireSession;
function FireSessions: TFireSessions;

procedure ExchangeFieldType(qry: TFireQuery; fld: string; ftClass: TFieldClass);
{$ENDIF}

implementation

{$IFDEF FIREDAC}
{ .$R *.dfm }

uses
  FireDAC.Phys.Intf,

{$IFDEF MSWINDOWS}
{$IFNDEF BPL}
  IniFilesEx,  // TODO -oAL : reavaliar - não deveria ter referencia para codigo interno
  uDebug,      // TODO -oAL : refatorar, para não ter dependencia da uDebug
  FireDacLoginDialogBase,
{$ENDIF}
  FireDAC.Stan.Util,
  FireDAC.Phys.SQLiteVDataSet,
  FireDAC.Stan.ExprFuncs,
  Registry,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.ODBC,
  FireDAC.Phys.Oracle, FireDAC.Phys.MySQL,
  FireDAC.Moni.RemoteClient,
  FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.DApt, FireDAC.Phys.IBBase,
  FireDAC.Phys.FB, FireDAC.Phys.MSSQL,

{$IF CompilerVersion>27} // >XE6
  FireDAC.Phys.MySQLDef,
  FireDAC.Phys.ODBCDef, FireDAC.Phys.OracleDef,
{$IFEND}
{$ENDIF}
{$IFDEF FMX}
  FMX.Controls, FMX.Forms, FMX.Dialogs,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.FMXUI.Login, FireDAC.FMXUI.Error,
{$ELSE}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  FireDAC.VCLUI.Wait, FireDAC.Comp.UI,
  FireDAC.VCLUI.Login, FireDAC.VCLUI.Error,
{$ENDIF}
{$IF defined(ANDROID) or defined(IOS)}
  System.IOUtils,
{$IFEND}
  System.SyncObjs,
  FireDAC.Phys.SQLite;

{ TFireDatabase }

var
  LArqConfigINI: String;

type

  TFireDacDataModule = class(TComponent)
{$IFDEF MSWINDOWS}
    FDGUIxLoginDialog1: TFDGUIxLoginDialog;
    FDGUIxErrorDialog1: TFDGUIxErrorDialog;
    FDPhysODBCDriverLink1: TFDPhysODBCDriverLink;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    Script: TFDScript;
  private
{$IFDEF VCL}
    procedure FDGUIxLoginDialog1Login(ASender: TObject; var AResult: Boolean);
{$ENDIF}
{$ENDIF}
  private
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  public
    constructor Create(ow: TComponent);
    { Public declarations }
  end;

Var

  LSessions: TFireSessions;
  LSession: TFireSession;
  LFireDacDataModule: TFireDacDataModule;
  LLocalConnection: TFireDatabase;
  LLocalSQL: TFDLocalSQL;
  LOnFinalization: Boolean;
  LOnFinalizationED: Boolean;


{$IFDEF VER270}
{$IFDEF BDE2FIREDAC}

function Session: TSession;
begin
  result := TSession(FireSession);
end;

function Sessions: TSessions;
begin
  result := TSessions(FireSessions);
end;

function TSessions.FindSession(sSession: String): TSession;
begin
  result := TSession(FireSessions.FindSession(sSession));
end;
{$ENDIF}
{$ENDIF}

type

  TDatabasesList = class(TList)
  private
    // FLock: TCriticalSection;
    function GetItems(idx: Integer): TFireDatabase;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property items[idx: Integer]: TFireDatabase read GetItems;
    function FindDataBase(alias: String): TFireDatabase;
  end;

var
  LDatabases: TDatabasesList;

function FireSession: TFireSession;
begin
  result := LSession;
end;

function FireSessions: TFireSessions;
begin
  result := LSessions;
end;

procedure TFireDatabase.CloseDataSets;
var
  i: Integer;
begin
  for i := 0 to DataSetCount - 1 do
    DataSets[i].Close;
end;

constructor TFireDatabase.Create(ow: TComponent);
begin
  inherited;
  if not assigned(LDatabases) then
    exit;
  LDatabases.Lock;
  try
{$IFDEF MSWINDOWS}
{$IFDEF USACUSTOMDLG}
    OnLogin := DoLoginDialogEvent;
{$ELSE}
    LoginDialog := LFireDacDataModule.FDGUIxLoginDialog1;
{$ENDIF}
{$ENDIF}
    AfterConnect := DoAfterConnectEvent;
    LDatabases.Add(self);
    FSession := LSession;
    FormatOptions.InlineDataSize := 255;
    with FormatOptions do
    begin
      // FmtDisplayNumeric := '0.00';  // nao ficou bom (AL)
      // FmtEditNumeric:= '0.00';
      MaxStringSize := 255;
      OwnMapRules := true;
{      with MapRules.Add do
      begin      // 20/01/15 nao e aplica no ORACLE - dava erro no PDVSync ao enviar sig02 - dobra o tamanho de caracteres
        SourceDataType := dtByteString;
        TargetDataType := dtAnsiString;
      end;
      with MapRules.Add do  // nao e aplica no ORACLE
      begin
        SourceDataType := dtWideString;
        TargetDataType := dtAnsiString;
      end;
}
      with MapRules.Add do
      begin
        SourceDataType := dtDateTimeStamp;
        TargetDataType := dtDateTime;
      end;
      with MapRules.Add do
      begin
        SourceDataType := dtBCD;
        TargetDataType := dtDouble;
      end;

      with MapRules.Add do
      begin
        SourceDataType := dtFmtBCD;
        TargetDataType := dtCurrency;
      end;

      with MapRules.Add do
      begin
        SourceDataType := dtWideMemo;
        TargetDataType := dtMemo;
        // dtWideString;  -> cadastro de produtos (memos SYNOPSE)
      end;
      with MapRules.Add do
      begin
        SourceDataType := dtSingle;
        TargetDataType := dtDouble;
      end;
    end;
    FetchOptions.RowsetSize := 250; // menos de 50 repete muitas buscas... mais de 500 - fica pesado
    FetchOptions.Mode := fmOnDemand;
    FetchOptions.CursorKind := ckDynamic;
    FetchOptions.AutoClose := true; // teste

    ResourceOptions.AutoReconnect := true;

    with UpdateOptions do
    begin
      CheckUpdatable := false;
      CheckReadOnly := false;
    end;

  finally
    LDatabases.UnLock;
  end;
end;

destructor TFireDatabase.Destroy;
begin
  if assigned(LDatabases) then
  begin
    LDatabases.Lock;
    try
      LDatabases.Remove(self);
    finally
      LDatabases.UnLock;
    end;
  end;
  inherited;
end;

procedure TFireDatabase.DoAfterConnectEvent(sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    LoginPrompt := false;

  if sametext(Params.Values['DriverID'], 'mssql') then
    ExecSQL('set dateformat mdy;');

end;

procedure TFireDatabase.DoConnect;
begin
{$IFDEF MSWINDOWS}
  if Params.Values['DriverID'] = 'FB' then
  begin
    if Params.Values['VendorLib'] <> '' then
      if LFireDacDataModule.FDPhysFBDriverLink1.VendorLib <> Params.Values['VendorLib'] then
      begin
        LFireDacDataModule.FDPhysFBDriverLink1.Release;
        LFireDacDataModule.FDPhysFBDriverLink1.VendorLib := Params.Values['VendorLib'];
      end;
  end;
{$ENDIF}
  if Params.Values['DriverID'] = '' then
    raise exception.Create('Falta definir o protocolo em ' + LArqConfigINI + ' para a conexão: ' + Databasename + ' Alias: ' + AliasName);
  inherited;
end;

{$IFDEF USACUSTOMDLG}

procedure TFireDatabase.DoLoginDialogEvent(AConnection: TFDCustomConnection; const AConnectionDef: IFDStanConnectionDef);
var
  dlg: TFireDacLoginDlgBase;
begin
  dlg := TFireDacLoginDlgBase.Create(nil);
  try
    dlg.Connection := AConnection;
    dlg.Caption := 'Login: ' + Databasename;
    dlg.User_Name.Text := AConnection.Params.Values['USER_NAME'];
    dlg.ShowModal;
    if dlg.Canceled = false then
    begin
{$IFDEF VER280}
      AConnectionDef.Params.UserName := dlg.User_Name.Text;
      AConnectionDef.Params.Password := dlg.Password.Text;
{$ELSE}
      AConnectionDef.UserName := dlg.User_Name.Text;
      AConnectionDef.Password := dlg.Password.Text;
{$ENDIF}
    end;
  finally
    dlg.Free;
  end;
end;
{$ENDIF}

procedure TFireDatabase.GetColumnNames(ATable, aTmp: string; ALst: TStrings);
begin
  GetFieldNames('', '', ATable, '', ALst);
end;

function TFireDatabase.GetDatabasename: string;
begin
  result := ConnectionName;
end;

procedure TFireDatabase.GetFieldNames(tabela: string; lst: TStrings);
begin
  GetColumnNames(tabela, '', lst);
end;

procedure TFireDatabase.GetTableNames(lst: TStrings; sysObjs: Boolean);
var
  LScopes: TFDPhysObjectScopes;
begin
  LScopes := [osMy, osOther];
  if sysObjs then
    LScopes := LScopes + [osSystem];
  inherited GetTableNames('', '', '', lst, LScopes, [tkTable, tkView]);
end;

{$IFDEF MSWINDOWS}

function GetComputerName: String;
var
  LBuffer: array [0 .. 255] of Char;
  LSize: dWord;
  LComputerName: string;
begin
  with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('\System\CurrentControlSet\Control\ComputerName\ComputerName', false);
      result := ReadString('ComputerName');
    finally
      Free;
    end;

  { Quando a função acima não retornar o nome do computador, e executada nova
    função para ler o nome do computador - Acessibilidade do Usuário do Windows
    ou Windows Service }
  if Trim(result) = '' then
  begin
    LSize := MAX_COMPUTERNAME_LENGTH + 1;
    Windows.GetComputerName(LBuffer, LSize);
    LComputerName := LBuffer;
    result := StrPas(PChar(LComputerName));
  end;
end;
{$ENDIF}

procedure FillLocalSQLConnectionParams(DB: TFireDatabase);
begin
  DB.DriverName := 'SQLite';
  DB.Params.Add('Database=:memory:');
  DB.ConnectionName := c_alias_Local_SQLite;
  DB.FDriveType := 'SQLITE';
  DB.LoginPrompt := false;
end;

function Protocol2DriverID(Prot: string): string;
begin
  if pos('firebird', Prot) > 0 then
    result := 'FB'
  else if pos('oracle', Prot) > 0 then
    result := 'Ora'
  else if (pos('ado', Prot) > 0) or (pos('odbc', Prot) > 0) then
    result := 'ODBC'
  else if pos('mssql', Prot) > 0 then
    result := 'MSSQL'
  else if sametext(Prot, 'mysql') then
    result := 'MySQL'

  else
    result := Prot;
end;

procedure InitDataBaseParams(DB: TFireDatabase; inMemory:boolean=false);
var
  ini: TIniFIle;
  str: TStringList;
  OSAuthent,Prot, porta, serv, bc, us, pw, dialect, cSet, CharacterSet, VendorLib: string;
  ARowsetsize: Integer;
  AAutoClose: Boolean;
  // oDef: IFDStanConnectionDef;
  procedure FireFillParams(ADriveName: string; Params: TStrings);
    procedure Add(nome: string; valor: string);
    begin
      // if DB.Params.IndexOfName(nome) < 0 then
      // DB.Params.Values[nome] := valor;
      Params.Values[nome] := valor;
    end;
    function appRoleName: string;
    begin
      result := StringReplace(uppercase(ExtractFileName(ParamStr(0))), 'XE.EXE', '.', []);
      result := copy(result, 1, pos('.', result) - 1);
    end;

  begin
{$IFDEF MSWINDOWS}
    Add('DriverID', ADriveName);

    if sametext(ADriveName, 'FB') then
    begin
      Add('Server', serv);
      Add('Database', bc);
      Add('OSAuthent',OSAuthent);
      if VendorLib <> '' then
      begin
        LFireDacDataModule.FDPhysFBDriverLink1.VendorLib := VendorLib;
        Add('VendorLib', VendorLib);
      end;
      Add('RoleName', appRoleName);
      if porta <> '' then
        Add('Port', porta);
      if dialect <> '' then
        Add('SQLDialect', dialect);
      if CharacterSet <> '' then
         Add('CharacterSet', CharacterSet); //Compatibilidade com os campos Nome no banco WIN1252 - Calixto
      DB.FDriveType := 'INTRBASE';
    end
    else if sametext(ADriveName, 'Ora') then
    begin
      Add('Database', bc);
      Add('OSAuthent',OSAuthent);
      Add('AuthMode','Normal');
      if CharacterSet <> '' then
         Add('CharacterSet', CharacterSet); //UTF-8  ou  <NLS_LANG>
      DB.FDriveType := 'ORACLE';
    end
    else if sametext(ADriveName, 'ODBC') then
    begin
      Add('Datasource', bc);
      DB.FDriveType := 'ODBC';
    end
    else if sametext(ADriveName, 'MSSQL') then
    begin
      Add('Server', serv);
      Add('Database', bc);
      Add('RoleName', appRoleName);
      DB.FDriveType := 'MSSQL';
    end
    else if sametext(ADriveName, 'MySQL') then
    begin
      Add('Database', bc);
      Add('Server', serv);
      if porta = '' then
        porta := '3306';
      Add('Port', porta);
      DB.FDriveType := 'MYSQL';
    end;
{$ENDIF}
    Add('ApplicationName', ExtractFileName(ParamStr(0)));
{$IFDEF MSWINDOWS}
    Add('Workstation', GetComputerName());
{$ENDIF}
    if us <> '' then
      Add('User_Name', us);
    if pw <> '' then
      Add('Password', pw);

{    if
       (inMemory  and (not sametext(ADriveName,'SQLITE') ))
       or
     (fileExists('debug.on')) or FindCmdLineSwitch('D', ['-', '\', '/'], true) then
      Add('MonitorBy', 'Remote');
}
  end;

begin

  if (sametext(DB.AliasName, c_alias_Local_SQLite)) then
  begin
    FillLocalSQLConnectionParams(DB);
    exit;
  end;

  if not fileExists(LArqConfigINI) then
    exit;

  ini := TIniFIle.Create(LArqConfigINI);
{$IFDEF LOG}
  debugLog('Arq.Config Firedac: ' + LArqConfigINI + ' Databasename: ' + DB.AliasName);
{$ENDIF}
  str := TStringList.Create;
  try

    ini.ReadSectionValues(DB.AliasName, str);
    if (str.count = 0) then
      raise exception.Create('Não leu informações de configuração: ' + LArqConfigINI + ' Alias: ' + DB.AliasName);

    if not ini.ValueExists(DB.AliasName, 'AutoClose') then
    begin
      ini.WriteBool(DB.AliasName, 'AutoClose', true);
      ini.WriteInteger(DB.AliasName, 'RowSetSize', 250);
    end;

    AAutoClose := ini.ReadBool(DB.AliasName, 'AutoClose', true);
    ARowsetsize := ini.ReadInteger(DB.AliasName, 'RowSetSize', 250);
    Prot := lowercase(str.Values['Protocol']);
    porta := str.Values['Port'];
    serv := str.Values['Hostname'];
    bc := str.Values['Database'];
    us := str.Values['User'];
    pw := str.Values['Password'];
    dialect := str.Values['Dialect'];
    cSet := str.Values['CharacterSet'];
    CharacterSet := str.Values['CharacterSet'];
    VendorLib := str.Values['VendorLib'];
    OSAuthent := str.Values['OSAuthent'];

    if OSAuthent='' then
       OSAuthent := 'no';

    if Prot = '' then
      exit;

    DB.FetchOptions.AutoClose := AAutoClose;
    if ARowsetsize <= 0 then
    begin
      DB.FetchOptions.Mode := fmAll; // carrega todos os registros da tabela - indicado para ambientes instáveis
    end
    else
    begin
      DB.FetchOptions.RowsetSize := ARowsetsize;
      DB.FetchOptions.Mode := fmOnDemand; // carrega somente o numero de registro necessário.  Indicado para tabelas grandes
    end;

    FireFillParams(Protocol2DriverID(Prot), DB.Params);

    { oDef := FDManager.ConnectionDefs.FindConnectionDef(DB.AliasName);
      FireFillParams(Protocol2DriverID(Prot), oDef.Params);

      DB.ConnectionDefName := DB.AliasName;
      DB.Params.Assign(oDef.Params);
    }

  finally
    ini.Free;
    str.Free;
  end;
end;

procedure TFireDatabase.SetAliasName(const Value: string);
begin
  FAliasName := Value;
  InitDataBaseParams(self);
end;

procedure TFireDatabase.SetDatabasename(const Value: string);
var
  DB: TFireDatabase;
begin

  DB := Session.FindDataBase(Value);
  if assigned(DB) then
    raise exception.Create('Já existe um database com o mesmo identificador <' + Value + '>');
  ConnectionName := Value;

end;

procedure TFireDatabase.SetKeepConnection(const Value: Boolean);
begin
  FKeepConnection := Value;
end;

procedure TFireDatabase.SetSession(const Value: TFireSession);
begin
  FSession := Value;
end;

procedure TFireDatabase.SetSessionName(const Value: String);
begin
  FSessionName := Value;
end;

procedure TFireDatabase.SetTransIsolation(const Value: TFireTransIsolation);
begin
  FTransIsolation := Value;
end;

{ TFireDatabases }

{ TFireUpdateSql }

procedure TFireUpdateSql.Apply(UpdateKind: TUpdatekind);
var
  uk: TFDUpdateRequest;
  err: TFDErrorAction;
  opt: TFDUpdateRowOptions;
begin
  case UpdateKind of
    ukModify:
      uk := arUpdate;
    ukInsert:
      uk := arInsert;
    ukDelete:
      uk := arDelete;
  end;
  inherited Apply(uk, err, opt);
end;

constructor TFireUpdateSql.Create(ow: TComponent);
begin
  inherited;
end;

destructor TFireUpdateSql.Destroy;
begin

  inherited;
end;

procedure TFireUpdateSql.ExecSQL(UpdateKind: TUpdatekind);
var
  q: TFireQuery;
begin
{$IFDEF LOG}
  q := TFireQuery(Query[UpdateKind]);
  debugLog(q.SQL.Text);
{$ENDIF}
  TFireQuery(Query[UpdateKind]).ExecSQL;
{$IFDEF LOG}
  debugLog('RowsAffected: ' + IntToStr(q.RowsAffected));
{$ENDIF}
end;

function TFireUpdateSql.GetDatabasename: String;
begin
  result := inherited ConnectionName;
end;

function TFireUpdateSql.GetDataset: TDataSet;
begin
  result := inherited DataSet;
end;

function TFireUpdateSql.GetDeleteSQL: TStrings;
begin
  result := inherited DeleteSQL;
end;

function TFireUpdateSql.GetInsertSQL: TStrings;
begin
  result := inherited InsertSQL;
end;

function TFireUpdateSql.GetModifySQL: TStrings;
begin
  result := inherited ModifySQL;
end;

function TFireUpdateSql.GetQuery(kd: TUpdatekind): TDataSet;
begin
  if not assigned(FQuery[kd]) then
    FQuery[kd] := TFireQuery.Create(self);
  result := FQuery[kd];
end;

function TFireUpdateSql.GetSQL(UpdateKind: TUpdatekind): TStrings;
begin
  result := TFireQuery(GetQuery(UpdateKind)).SQL;
end;

procedure TFireUpdateSql.SetDatabasename(const Value: String);
begin
  inherited ConnectionName := Value;
end;

procedure TFireUpdateSql.SetDataset(const Value: TDataSet);
begin
  inherited DataSet := TFDAdaptedDataSet(Value);
end;

procedure TFireUpdateSql.SetDeleteSQL(const Value: TStrings);
begin
  inherited DeleteSQL.Assign(Value);
  TFireQuery(Query[ukDelete]).SQL.Assign(Value);
end;

procedure TFireUpdateSql.SetInsertSQL(const Value: TStrings);
begin
  inherited InsertSQL.Assign(Value);
  TFireQuery(Query[ukInsert]).SQL.Assign(Value);
end;

procedure TFireUpdateSql.SetModifySQL(const Value: TStrings);
begin
  inherited InsertSQL.Assign(Value);
  TFireQuery(Query[ukModify]).SQL.Assign(Value);
end;

procedure TFireUpdateSql.SetParams(UpdateKind: TUpdatekind);
begin
  TFireQuery(Query[UpdateKind]).Params.AssignValues(TFireQuery(DataSet).Params);
end;

procedure TFireUpdateSql.SetParams(ADataset: TDataSet; UpdateKind: TUpdatekind);
begin
  TFireQuery(Query[UpdateKind]).Params.AssignValues(TFireQuery(ADataset).Params);
end;

{ TFireQuery }

function TFireQuery.BookmarkValid(Bookmark: TBookMark): Boolean;
begin
  result := Bookmark <> nil;
  if UniDirectional then
    result := false;
end;

constructor TFireQuery.Create(owner: TComponent);
begin
  inherited;
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := SqlChanged;
  CachedUpdates := true;
  ParamCheck := true;
  RequestLive := false;
  inherited OnUpdateError := DoUpdateError;
  inherited OnReconcileError := DoReconcileError;
  {$ifdef LOCALSQL}
     usaLocalSql := true; // ativado por diretiva
  {$endif}
end;

destructor TFireQuery.Destroy;
var
  DB: TFireDatabase;
begin
  DB := nil;
  (*
{$IFNDEF MULTI_THREADED}
 if not sametext(Databasename,c_alias_Local_SQLite) then
 begin
  if assigned(LSession) then
    DB := LSession.FindDataBase(Databasename);
  if DB <> nil then
    if DB.FetchOptions.AutoClose and (DB.DataSetCount <= 1) and (DB.Connected) then
      DB.Offline;
 end;
{$ENDIF}*)
  LocalSQL := nil;
  FreeAndNil(FSQL);

  inherited;
end;

procedure TFireQuery.ExecDirect;
begin
  // UpdateOptions.
end;

procedure TFireQuery.Execute(ATimes, AOffset: Integer);
begin
{$IFDEF LOG}
  debugLog(SQL.Text);
{$ENDIF}
  inherited;
{$IFDEF LOG}
  debugLog('RowsAffected: ' + IntToStr(RowsAffected));
{$ENDIF}
end;

procedure TFireQuery.FreeBookMark(book: TBookMark);
begin
  if not UniDirectional then
    inherited FreeBookMark(book);
end;

function TFireQuery.GetBookMark: TBookMark;
begin
  result := nil;
  if not UniDirectional then
    result := inherited GetBookMark;
end;

function TFireQuery.GetChacedUpdates: Boolean;
begin
  result := inherited CachedUpdates;
end;

function TFireQuery.GetDatabase: TFireDatabase;
begin
  result := TFireDatabase(inherited Connection);
end;

function TFireQuery.GetDataSource: TDataSource;
begin
  result := MasterLink.DataSource;
end;

function TFireQuery.GetRequestLive: Boolean;
begin
  result := UpdateOptions.RequestLive;
end;

function TFireQuery.GetSQL: TStrings;
begin
  result := FSQL;
end;

function TFireQuery.GetUniDirectional: Boolean;
begin
  result := FetchOptions.UniDirectional;
end;

function TFireQuery.GetUpdateMode: TUpdateMode;
begin
  result := UpdateOptions.UpdateMode;
end;

function TFireQuery.GetUpdateObject: TFireUpdateSql;
begin
  result := TFireUpdateSql(inherited UpdateObject);
end;

procedure TFireQuery.GotoBookmark(book: TBookMark);
begin
  if (not UniDirectional) and Active then
    inherited GotoBookmark(book);
end;

function TFireQuery.GetAfterInsert:TDataSetNotifyEvent;
begin
  result := inherited AfterInsert;
end;

procedure TFireQuery.SetAfterInsert(value:TDataSetNotifyEvent);
begin
  inherited AfterInsert := value;
end;


procedure TFireQuery.InitNullParams;
var
  x: Integer;
begin
  for x := 0 to ParamCount - 1 do
    if Params[x].DataType = ftUnknown then
    begin
      Params[x].DataType := ftString;
      Params[x].Clear;
    end;
end;

procedure TFireQuery.InternalClose;
begin
  LocalSQL := nil;
  if CachedUpdates and UpdatesPending then
    ApplyUpdates();
  inherited;
end;

procedure TFireQuery.InternalOpen;
begin
  inherited;

end;

procedure TFireQuery.InternalPost;
begin

  inherited;

end;

procedure TFireQuery.Prepare;
begin
  InitNullParams;
  inherited Prepare;
end;

function TFireQuery.RowsAffected: Integer;
begin
  result := inherited RowsAffected;
end;

procedure TFireQuery.DoUpdateError(ASender: TDataSet; AException: EFDException; ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
  var AAction: TFDErrorAction);
begin
  FErrorMessage := AException.message;
  inc(FErrorCount);
  AAction := eaFail;
  if assigned(FOnUpdateError) then
    FOnUpdateError(ASender, AException, ARow, ARequest, AAction);
end;

procedure TFireQuery.DoReconcileError(DataSet: TFDDataSet; E: EFDException; UpdateKind: TFDDatSRowState; var Action: TFDDAptReconcileAction);
begin

  FErrorMessage := E.message;
  inc(FErrorCount);
  Action := raAbort;
  if assigned(FOnReconcileError) then
    FOnReconcileError(DataSet, E, UpdateKind, Action);
  // no alquery, o undo é executado se estiver em ManualUpdate = false
  // se estiver com ManualUpdate = true, não é feito automatico.
  // UndoLastChange(True);  // movido para o TAlQuery

end;

procedure TFireQuery.SetActive(AValue: Boolean);
begin
  if AValue then
  begin

    if sametext(Databasename, c_alias_Local_SQLite) then
      LocalSQL := nil
    else
     if useLocalSql then
     begin
        if LLocalSQL.active=false then
           LLocalSQL.active := true;
        LocalSQL := LLocalSQL;  // usado nos relatorios
     end;

    if FDatabasename = '' then
      Databasename := c_alias_Local_SQLite; // usa SQLITE memory
  end;
{$IFDEF LOG}
  if AValue then
  begin
    if assigned(Connection) then
      debugLog('Driver ID: ' + Connection.DriverName);
    // + ' Database: '+Self.database. ['Hostname']+':'+Self.Params.ParamValues['Database'] );
    debugLog('Alias: ' + Databasename + ' SQL: ' + SQL.Text);
  end;
{$ENDIF}

{  AL - comentado porque não é igual em todos os pontos. - Relatorio do encmenda não funcionou.  F3/F5 - enc por data. (.DB)
        Talves seja necessário tratar o ponto onde é contruido o codigo que chama.

  if sametext(FDatabasename, c_alias_Local_SQLite) and (UpdateOptions.KeyFields = '') then
    UpdateOptions.KeyFields := 'ROWID';
}

  inherited SetActive(AValue);
{$IFDEF LOG}
  if AValue then
    debugLog('RowsAffected: ' + IntToStr(RowsAffected));
{$ENDIF}
end;

procedure TFireQuery.SetAutoRefresh(const Value: Boolean);
begin
  FAutoRefresh := Value;
end;

procedure TFireQuery.SetCachedUpdates(const Value: Boolean);
begin
  inherited CachedUpdates := Value;
end;

procedure TFireQuery.SetDatabase(const Value: TFireDatabase);
begin
  inherited Connection := Value;
  if assigned(Value) then
    FDatabasename := Value.Databasename;

end;

procedure TFireQuery.SetDatabasename(const Value: string);
var
  ss: TFireSession;
begin

  System.TMonitor.Enter(self);
  try
    // LLock.Acquire;

    FDatabasename := Value;

    if FDatabasename <> '' then
    begin
      ss := FireSessions.FindSession(SessionName);
      if not assigned(ss) then
        ss := FireSession;
      Database := ss.FindDataBase(Value);

      if assigned(database) and (database.FDriveType ='') then
         InitDataBaseParams(database);

      ConnectionName := Value;


{$IFDEF LOCALSQL}
      if sametext(Value, c_alias_Local_SQLite) then
        LocalSQL := nil;
{$ENDIF}
    end;

  finally
    // LLock.Release;
    System.TMonitor.exit(self);
  end;
end;

procedure TFireQuery.SetDatasource(const Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError('Circular Data Link', self);
  MasterLink.DataSource := Value;
end;

procedure TFireQuery.SetOnUpdateError(const Value: TFDUpdateErrorEvent);
begin
  FOnUpdateError := Value;
end;

procedure TFireQuery.SetParamCheck(const Value: Boolean);
begin
  FParamCheck := Value;
end;

procedure TFireQuery.SetRequestLive(const Value: Boolean);
begin
  UpdateOptions.RequestLive := Value; // -- testar ReadOnly, quando executa como false
  UpdateOptions.EnableDelete := true;
  UpdateOptions.EnableInsert := true;
  UpdateOptions.EnableUpdate := true;
  // if value then
  // FetchOptions.Items := FetchOptions.Items + [fiMeta]

end;

procedure TFireQuery.SetSessionName(const Value: String);
begin
  FSessionName := Value;
end;

procedure TFireQuery.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

procedure TFireQuery.SetUniDirectional(const Value: Boolean);
begin
  FetchOptions.UniDirectional := Value;
  // quando usa unidictional nao pode usar BookMark
end;

procedure TFireQuery.SetUpdateMode(const Value: TUpdateMode);
begin
  UpdateOptions.UpdateMode := Value;
end;

procedure TFireQuery.SetUpdateObject(const Value: TFireUpdateSql);
begin
  inherited UpdateObject := Value;
  if assigned(Value) then
  begin
    Value.Connection := self.Connection;
    Value.DataSet := self;
  end;
end;

procedure TFireQuery.SqlChanged(sender: TObject);
var
  s: String;
  r: string;
  i, f: Integer;
begin
  // troca o modelo de filtro para o modelo de macros permitidos no Firedac
  s := TStringList(sender).Text;
  repeat
    i := pos('{', s);
    if i > 0 then
    begin // é um filtro
      r := copy(s, i, length(s));
      f := pos('}', r);
      if f > 0 then
      begin // finalizou o filtro
        r := copy(r, 1, f); // troca o filtro para macro do firedac
        s := StringReplace(s, r, '!' + copy(r, 2, length(r) - 2), []);
      end
      else
        i := 0;
    end;
  until i = 0;
  inherited SQL.Text := s;
  // atribui ao componente o sql convertido para macros.
end;

{ TFireSessions }

function TFireSessions.count: Integer;
begin
  result := 1;
end;

constructor TFireSessions.Create(own: TComponent);
begin
  inherited;
  FAliases := TStringList.Create;
{$IFDEF MSWINDOWS}
  TFDPhysFBDriverLink.Create(self);
{$ENDIF}
  TFDGUIxWaitCursor.Create(self);
end;

{$IFDEF BPL}

function GetProgramFilesDir: String;
begin
  with TRegistry.Create(KEY_QUERY_VALUE) do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion', true);
    result := ReadString('ProgramFilesDir');
    Free;
  end;
end;
{$ENDIF}

function GetConfigIni: string;
begin
  if LArqConfigINI = '' then
{$IFDEF MSWINDOWS}
{$IFDEF BPL}
    LArqConfigINI := GetProgramFilesDir + '\store\config\zConnections.ini';
{$ELSE}
    LArqConfigINI := GetIniFilesDir + 'zConnections.ini';
{$ENDIF}
{$ELSE}
    LArqConfigINI := TPath.Combine(TPath.GetSharedDocumentsPath, 'zConnections.ini');
{$ENDIF}
  result := LArqConfigINI;

end;

procedure TFireSessions.LoadConfig;
var
  ini: TIniFIle;
  oDef: IFDStanConnectionDef;
  i: Integer;
  prm: TStringList;
begin

  GetConfigIni;

{$IFDEF MSWINDOWS}
  { FDManager.Close;
    FDManager.ConnectionDefFileAutoLoad := false;
    if FDManager.ConnectionDefFileName <> LArqConfigINI + '_def' then
    FDManager.ConnectionDefFileName := LArqConfigINI + '_def';
  }
{$ENDIF}
  FAliases.Clear;
  ini := TIniFIle.Create(LArqConfigINI);
  prm := TStringList.Create;
  try
    ini.ReadSections(FAliases);
    { .$IFDEF LOCALSQL }

    for i := 0 to FAliases.count - 1 do
    begin
      // prm.Values['ConnectionDef'] := FAliases[i];
      prm.Values['Name'] := FAliases[i];
      prm.Values['DriverID'] := Protocol2DriverID(ini.ReadString(FAliases[i], 'Protocol', ''));
      prm.Values['Server'] := ini.ReadString(FAliases[i], 'Hostname', '');
      prm.Values['Database'] := ini.ReadString(FAliases[i], 'Database', '');
      prm.Values['OSAuthent'] := ini.ReadString(FAliases[i],'OSAuthent','no');
      prm.Values['Pooled'] := 'False';
{$IFDEF MULTI_THREADED}
      // prm.Values['Pooled'] := 'True';   // nao funcionou;
{$ELSE}
{$ENDIF}
{$IFDEF MSWINDOWS}
      // FDManager.AddConnectionDef(FAliases[i], prm.Values['DriverID'], prm, true);
{$ENDIF}
    end;
    FAliases.Add(c_alias_Local_SQLite);

    { .$ENDIF }
  finally
    ini.Free;
  end;

{$IFDEF MSWINDOWS}
  // FDManager.Open;
{$ENDIF}
end;

destructor TFireSessions.Destroy;
begin
  FAliases.Free;
  inherited;
end;

function TFireSessions.FindSession(sSession: String): TFireSession;
begin
  result := LSession;
end;

function TFireSessions.GetCurrentSession: TFireSession;
begin
  result := LSession;
end;

function TFireSessions.GetItems(idx: Integer): TFireSession;
begin
  result := LSession;
end;

function TFireSessions.GetList(sSession: string): TFireSession;
begin
  result := FindSession(sSession);
end;

function TFireSessions.IsAlias(sAlias: String): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to FAliases.count - 1 do
    if sametext(FAliases[i], sAlias) then
    begin
      result := true;
      exit;
    end;
end;

procedure TFireSessions.OpenSession(sSession: String);
begin

end;

procedure TFireSessions.SetCurrentSession(const Value: TFireSession);
begin
  LSession := Value;
end;

procedure TFireSessions.SetItems(idx: Integer; const Value: TFireSession);
begin

end;

procedure TFireSessions.SetList(sSession: string; const Value: TFireSession);
begin
  LSession := Value;
end;

{ TFireSession }

procedure TFireSession.AddAlias(sAlias: string; driver: string; List: TStrings);
var
  i: Integer;
begin
  FConfigFile.WriteString(sAlias, 'Protocol', driver);
  for i := 0 to List.count - 1 do
    FConfigFile.WriteString(sAlias, List.Names[i], List.ValueFromIndex[i]);
end;

procedure TFireSession.AddDriver(const Name: string; List: TStrings);
begin

end;

procedure TFireSession.AddStandardAlias(alias, path, driver: string);
var
  h, d: string;
  i: Integer;
begin
  FConfigFile.WriteString(alias, 'Protocol', driver);
  i := pos(':', path);
  if i > 2 then
  begin
    h := copy(path, 1, i - 2);
    d := copy(path, i + 1, 255);
    FConfigFile.WriteString(alias, 'Hostname', h);
    FConfigFile.WriteString(alias, 'Database', d);
  end
  else
    FConfigFile.WriteString(alias, 'SERVER NAME', path);
end;

procedure TFireSession.Close;
var
  i: Integer;
begin
  for i := 0 to DatabaseCount - 1 do
    with Databases[i] do
    begin
      { .$IFDEF LOCALSQL }
      if not sametext(Databasename, c_alias_Local_SQLite) then { .$ENDIF }
        Offline;
      if LOnFinalization then
        Close;
    end;
end;

procedure TFireSession.CloseDatabase(lDb: TFireDatabase);
begin
  { .$IFDEF LOCALSQL }
  if not sametext(lDb.Databasename, c_alias_Local_SQLite) then { .$ENDIF }
    lDb.Offline;
  if LOnFinalization then
    lDb.Close;
end;

procedure TFireSession.CloseDatabases;
var
  i: Integer;
begin
  for i := 0 to DatabaseCount - 1 do
    with Databases[i] do
    begin
      { .$IFDEF LOCALSQL }
      if not sametext(Databasename, c_alias_Local_SQLite) then { .$ENDIF }
        Offline;
      if LOnFinalization then
        Close;
    end;
end;

constructor TFireSession.Create(ow: TComponent);
begin
  inherited;
  GetConfigIni;
  FConfigFile := TMemIniFile.Create(LArqConfigINI);
  FnetFileDir := ExtractFilePath(LArqConfigINI);
end;

function TFireSession.DatabaseCount: Integer;
begin
  result := 0;
  if assigned(LDatabases) then
    result := LDatabases.count;
end;

procedure TFireSession.DeleteAlias(sAlias: string);
begin
  FConfigFile.EraseSection(sAlias);
end;

destructor TFireSession.Destroy;
begin
  FConfigFile.Free;
  inherited;
end;

function TFireSession.FindDataBase(alias: String): TFireDatabase;
begin
  result := nil;
  // LLock.Acquire;
  System.TMonitor.Enter(self);
  try
    if assigned(LDatabases) then
      result := LDatabases.FindDataBase(alias);
  finally
    // LLock.Release;
    System.TMonitor.exit(self);
  end;
end;

function TFireSession.GetAliasDriverName(sAlias: String): String;
var
  ini: TIniFIle;
  DB: TFireDatabase;
  Prot: String;
begin
  DB := FindDataBase(sAlias);
  result := '';
  if assigned(DB) then
  begin
    Prot := DB.FDriveType;
    if sametext('FB', Prot) then
      result := 'INTRBASE'
    else if sametext('ORA', Prot) then
      result := 'ORACLE'
    else if sametext('MSSQL', Prot) then
      result := 'MSSQL'
    else
      result := Prot
  end
  else
  begin
    Prot := lowercase(FConfigFile.ReadString(sAlias, 'Protocol', ''));
    if pos('firebird', Prot) > 0 then
      result := 'INTRBASE'
    else if pos('oracle', Prot) > 0 then
      result := 'ORACLE'
    else if pos('mssql', Prot) > 0 then
      result := 'MSSQL'
  end;
end;

procedure TFireSession.GetAliasNames(items: TStrings);
begin
  items.Assign(FireSessions.FAliases);
end;

procedure TFireSession.GetAliasParams(sAlias: String; str: TStrings);
var
  DB: TFireDatabase;
  i: Integer;
  s: string;
  // oDef: IFDStanConnectionDef;
begin
  DB := FindDataBase(sAlias);
  str.Clear;
  if DB = nil then
  begin
    if assigned(str) then
      try
        FConfigFile.ReadSection(sAlias, str);
        for i := 0 to str.count - 1 do
        begin
          s := str[i];
          s := s + '=' + FConfigFile.ReadString(sAlias, s, '');
          str[i] := s;
        end;
        str.Add('SERVER NAME=' + str.Values['Server'] + ':' + str.Values['Database']);
      finally
      end;
  end
  else if assigned(str) then
  begin
{$IFDEF MSWINDOWS}
    { oDef := FDManager.ConnectionDefs.FindConnectionDef(sAlias);
      if assigned(oDef) then
      str.AddStrings(oDef.Params)
      else
    } str.AddStrings(DB.Params);
    str.Values['SERVER NAME'] := str.Values['Server'] + ':' + str.Values['Database'];
{$ENDIF}
  end;
end;

procedure TFireSession.GetDatabaseNames(L: TStrings);
var
  i: Integer;
begin
  if assigned(L) then
    L.Assign(FireSessions.FAliases);

  if assigned(LDatabases) then
    for i := 0 to LDatabases.count - 1 do
    begin
      if LDatabases.items[i].Databasename <> '' then
        if L.IndexOf(LDatabases.items[i].Databasename) < 0 then
          L.Add(LDatabases.items[i].Databasename);
    end;

end;

function TFireSession.GetDatabases(i: Integer): TFireDatabase;
begin
  result := nil;
  if assigned(LDatabases) then
    result := LDatabases.items[i];
end;

procedure TFireSession.GetTableNames(DBName, msk: String; a, b: Boolean; Dest: TStrings);
var
  DB: TFireDatabase;
begin
  DB := FindDataBase(DBName);
  if assigned(DB) then
    DB.GetTableNames(Dest, false);
end;

function TFireSession.IsAlias(sAlias: String): Boolean;
begin
  result := LSessions.IsAlias(sAlias);
end;

procedure TFireSession.ModifyAlias(alias: String; ts: TStrings);
var
  i: Integer;
begin
  for i := 0 to ts.count - 1 do
    FConfigFile.WriteString(alias, ts.Names[i], ts.ValueFromIndex[i]);
end;

procedure TFireSession.Open;
begin
  if not Active then
    Active := true;
end;

function TFireSession.OpenDatabase(dBase: string): TFireDatabase;
var
  i: Integer;
  DB: TFireDatabase;
begin
  DB := FindDataBase(dBase);
  if not DB.Connected then
    DB.Connected := true;
  result := DB;
end;

procedure TFireSession.SaveConfigFile;
begin
  FConfigFile.UpdateFile;
end;

procedure TFireSession.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TFireSession.SetDatabases(i: Integer; const Value: TFireDatabase);
var
  DB: TFireDatabase;
begin
  { db := LDatabases.Items[i];
    if db<>Value then
    begin
    // talves possa Free..
    end else
    // LDatabases.Items[i] := Value;
  }
end;

procedure TFireSession.SetnetFileDir(const Value: String);
begin
  FnetFileDir := Value;
end;

procedure TFireSession.SetOnDbNotify(const Value: TFireDatabaseNotifyEvent);
begin
  FOnDbNotify := Value;
end;

procedure TFireSession.SetSessionName(const Value: String);
begin
  FSessionName := Value;
end;

{ TFireStoredProc }

function TFireStoredProc.ExecSQL: Integer;
begin
  inherited ExecProc;
  result := RowsAffected;
end;

function TFireStoredProc.GetDatabase: TFireDatabase;
begin
  result := TFireDatabase(inherited Connection);
end;

procedure TFireStoredProc.SetDatabase(const Value: TFireDatabase);
begin
  inherited Connection := Value;
end;

procedure TFireStoredProc.SetDatabasename(const Value: String);
var
  ss: TFireSession;
begin
  FDatabasename := Value;
  ss := FireSessions.FindSession(SessionName);
  if not assigned(ss) then
    ss := FireSession;
  Database := ss.FindDataBase(Value);
  ConnectionName := Value;

end;

procedure TFireStoredProc.SetSessionName(const Value: string);
begin
  FSessionName := Value;
end;

{ TFireTable }
function GetDefCampo(alias, campo, tipo: string; tam, Dec: Integer; isDBF: Boolean = false; notNull: Boolean = false; valorBase: string = '';
  mx: Integer = 0): string;
var
  r, s: string;
  sDriveName: string;
  databasetype: Integer;
const
  dbIntrBase = 0;
  dbSQLServer = 1;
  dbOracle = 2;
  dbAccess = 3;
  dbSQLite = 4;
  function iff(b: Boolean; v: Variant; f: Variant): Variant;
  begin
    result := v;
    if not b then
      result := f;
  end;

begin
  sDriveName := FireSession.GetAliasDriverName(alias);
  if sDriveName = 'INTRBASE' then
    databasetype := dbIntrBase
  else if sDriveName = 'MSSQL' then
    databasetype := dbSQLServer
  else if sDriveName = 'ORACLE' then
    databasetype := dbOracle
  else if sDriveName = 'SQLITE' then
    databasetype := dbSQLite;

  if (tipo[1] = 'N') and (tam <= 0) then
    tam := 15;
  s := campo + ' ';
  if isDBF then
    case tipo[1] of
      '@':
        begin
          tipo[1] := 'D';
          tam := 8;
        end;
      'T':
        begin
          tipo[1] := 'C';
          tam := 8;
        end;
      '$':
        begin
          tipo[1] := 'N';
          tam := 15;
          Dec := 4;
        end;
      'S', 'I', '+':
        begin
          tipo[1] := 'N';
          tam := 10;
          Dec := 0;
        end;
    end;
  case tipo[1] of
    'A', 'C':
      case databasetype of
        dbOracle:
          r := 'VarChar2(' + IntToStr(iff(tam <= 0, 1, tam)) + ')';
        // dbSQLite:
        // r := 'TEXT';
      else
        r := 'VarChar(' + IntToStr(iff(tam <= 0, 1, tam)) + ')';
      end;
    'G':
      begin
        case databasetype of
          dbOracle:
            r := 'VarChar2(36)';
          // dbSQLite:
          // r := 'TEXT';
        else
          r := 'VarChar(36)';
        end;
        case databasetype of
          dbOracle:
            valorBase := ' Sys_Guid() ';
        end;
      end;
    'D':
      case databasetype of
        dbAccess, dbSQLServer:
          r := 'DateTime ';
        dbOracle:
          r := 'TimeStamp ';
      else
        r := 'Date';
      end;

    'T':
      r := 'Time';
    '@':
      r := 'TimeStamp';
    'I':
      case databasetype of
        dbAccess:
          r := 'Integer ';
      else
        r := 'Integer ';
      end;
    'S':
      r := 'SmallInt';
    'M', 'B':
      case databasetype of
        dbOracle:
          r := 'NCHAR';
        dbIntrBase:
          r := 'blob SUB_TYPE 1 SEGMENT SIZE ' + IntToStr(iff(tam <= 0, 80, tam));
        dbSQLServer:
          r := 'text';
      else
        r := 'Memo';
      end;
    '$':
      r := 'Money ';
    'N':
      case databasetype of
        dbAccess, dbSQLServer:
          r := 'Float ';
        dbSQLite:
          r := 'REAL ';
        dbIntrBase:
          r := 'Float ';
      else
        r := 'Numeric(' + IntToStr(tam) + ',' + IntToStr(Dec) + ') ';
      end;
    'L':
      r := 'Boolean';
    '+':
      r := 'AutoInc';
  end;

  if (uppercase(campo) = 'IDBASE') and (databasetype = dbSQLServer) then
  begin
    if mx = 0 then
      mx := 1;
    r := ' Int identity(' + IntToStr(mx) + ',1) ';
    notNull := true;
    valorBase := '';
  end;

  if sametext(Trim(valorBase), '''now''') then
    // corrige default firebird para outro tipo de banco
    case databasetype of
      dbSQLServer, dbAccess:
        valorBase := ' GetDate() ';
      dbIntrBase:
        valorBase := ' ''now'' ';
      dbOracle:
        valorBase := ' SysDate ';
    end;

  if pos('SYSDATE', uppercase(valorBase)) > 0 then
    case databasetype of
      dbSQLServer, dbAccess:
        valorBase := ' GetDate() ';
      dbIntrBase:
        valorBase := ' ''now'' ';
      dbOracle:
        valorBase := ' SysDate ';
    end;

  result := s + r;

  case databasetype of
    dbOracle, dbIntrBase:
      if valorBase <> '' then
        result := result + ' default ' + valorBase;
  end;

  if notNull then
    result := result + ' not null '
  else
  begin
    case databasetype of
      dbIntrBase:
        ; // nao faz nada quando for interbase
      dbOracle, dbAccess, dbSQLServer:
        result := result + ' null ';
    end;
  end;

  case databasetype of
    dbOracle, dbIntrBase:
      ;
  else
    if valorBase <> '' then
      result := result + ' default ' + valorBase;
  end;

end;


procedure ShowTables(conn:TFireDatabase);
var lst : tStringList;
begin
    if conn=nil then exit;

    lst := tStringList.create;
    try
      conn.GetTableNames('','','',lst);
      showMessage(lst.Text);
    finally
      lst.Free;
    end;
end;

procedure TFireTable.CreateTable;
var
  s, ss, tipo, sTableName: string;
  i: Integer;
  tam, Dec: Integer;
  conn: TFireDatabase;
  criaTmp: Boolean;
begin

  criaTmp := false;

  sTableName := TableName;

  s := 'Create Table ' + sTableName + ' ( ';
  if criaTmp then
    s := 'Create Global Temporary Table ' + sTableName + ' ( ';

  ss := '';
  for i := 0 to FieldDefs.count - 1 do
  begin
    tipo := 'C';
    case FieldDefs[i].DataType of
      ftFloat, ftCurrency:
        tipo := 'N';
      ftDate, ftDateTime, ftTime:
        tipo := 'D';
    end;
    tam := FieldDefs[i].Size;
    Dec := FieldDefs[i].Precision;
    if ss <> '' then
      ss := ss + ', ';
    ss := ss + GetDefCampo(Databasename, FieldDefs[i].Name, tipo, tam, Dec);
  end;
  s := s + ss + ' )';

  if criaTmp then
    s := s + ' on Commit Preserve rows';

  if Databasename = '' then
    Databasename := c_alias_Local_SQLite;

  conn := FireSession.FindDataBase(Databasename);

  tableName := stringReplace(sTableName,'"','',[rfReplaceAll]); // retira as aspas para fazer a comparação se a tabela ja existe.
  try
  if inherited exists then
    conn.ExecSQL('Drop table ' + sTableName);
  conn.ExecSQL(s);
  finally
    TableName := sTableName;  // restaura o dado original;
  end;
end;

procedure TFireTable.DeleteTable;
begin
  Connection.ExecSQL('drop table ' + TableName);
end;

procedure TFireTable.EmptyTable;
begin
  while not eof do
    delete;
end;

function TFireTable.GetDatabase: TFireDatabase;
begin
  result := TFireDatabase(inherited Connection);
end;

function TFireTable.GetReadOnly: Boolean;
begin
  result := FReadOnly;
end;

function TFireTable.GetTableName: string;
begin
  result := inherited TableName;
end;

procedure TFireTable.SetActive(AValue: Boolean);
begin
  if (AValue) then
  begin
    if FDatabasename = '' then
    begin
      Databasename := c_alias_Local_SQLite;
      inherited TableName := QuotedStr(TableName);
    end;
    if sametext(FDatabasename, c_alias_Local_SQLite) and (UpdateOptions.KeyFields = '') then
      UpdateOptions.KeyFields := 'ROWID';
  end;
  inherited;
end;

procedure TFireTable.SetDatabase(const Value: TFireDatabase);
begin
  inherited Connection := Value;
end;

procedure TFireTable.SetDatabasename(const Value: String);
var
  ss: TFireSession;
begin
  FDatabasename := Value;
  if FDatabasename <> '' then
  begin
    ss := FireSessions.FindSession(SessionName);
    if not assigned(ss) then
    begin
      ss := FireSession;
    end;
    Database := ss.FindDataBase(FDatabasename);
  end;
end;

procedure TFireTable.SetDefaultIndex(const Value: Boolean);
begin
  FDefaultIndex := Value;
end;

procedure TFireTable.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TFireTable.SetSessionName(const Value: string);
begin
  FSessionName := Value;
end;

procedure TFireTable.SetTableName(const Value: string);
var
  sOld: string;
begin
  sOld := UpdateOptions.UpdateTableName;
  inherited TableName := Value;
  if csDesigning in ComponentState then
  begin
    if sOld = '' then
      UpdateOptions.UpdateTableName := '';
  end;

end;

procedure TFireTable.SetTableType(const Value: TFireTableType);
begin
  FTableType := Value;
end;

procedure TFireTable.SetUpdateMode(const Value: TUpdateMode);
begin
  FUpdateMode := Value;
end;

{ TFireBatchMove }
{$IFDEF MSWINDOWS}

procedure TFireBatchMove.Execute;
begin
  RecordCount := inherited Execute;
end;

function TFireBatchMove.GetDestination: TFireTable;
begin
  result := TFireTable(inherited Destination);
end;

function TFireBatchMove.GetSource: TDataSet;
begin
  result := inherited Source;
end;

procedure TFireBatchMove.SetDestination(const Value: TFireTable);
begin
  inherited Destination := Value;
end;

procedure TFireBatchMove.SetMode(const Value: TFireBatchMode);
begin
  FMode := Value;
  case Value of
    batAppend:
      inherited Mode := dmAppend;
    batUpdate:
      inherited Mode := dmUpdate;
    batAppendUpdate:
      inherited Mode := dmAppendUpdate;
    batDelete:
      inherited Mode := dmDelete;
    batCopy:
      inherited Mode := dmAlwaysInsert;
  end;
end;

procedure TFireBatchMove.SetRecordCount(const Value: Integer);
begin
  FRecordCount := Value;
end;

procedure TFireBatchMove.SetSource(const Value: TDataSet);
begin
  inherited Source := Value;
end;

procedure TFireBatchMove.SetTableType(const Value: TFireTableType);
begin
  FTableType := Value;
end;
{$ENDIF}
{ TDatabasesList }

constructor TDatabasesList.Create;
begin
  inherited;
  // FLock := TCriticalSection.Create;
end;

destructor TDatabasesList.Destroy;
begin
  // FreeAndNil(FLock);
  inherited;
end;

// var
// FDGUIxLoginDialog1: TFDGUIxLoginDialog;

function TDatabasesList.FindDataBase(alias: String): TFireDatabase;
var
  i: Integer;
begin
  Lock;
  try
    result := nil;
    { .$IFDEF LOCALSQL }
    if sametext(alias, c_alias_Local_SQLite) then
    begin
      result := LLocalConnection;
      exit;
    end;
    { .$ENDIF }
    for i := 0 to count - 1 do
      if sametext(items[i].Databasename, alias) then
      begin
        result := items[i];
        exit;
      end;
    if not assigned(result) then
      if FireSessions.FAliases.IndexOf(alias) >= 0 then
      begin
        // é um alias que não foi iniado como Databasename
        result := TFireDatabase.Create(FireSession);
        result.AliasName := alias;
        result.ConnectionName := alias; // trick para quebrar loop;
        // InitDataBaseParams(result);  -- a chamada é feita ao atribuir o aliasname
        // result.LoginDialog := LFireDacDataModule.FDGUIxLoginDialog1;
        result.LoginPrompt := true;
      end;

  finally
    UnLock;
  end;
end;

function TDatabasesList.GetItems(idx: Integer): TFireDatabase;
begin
  result := TFireDatabase(inherited Get(idx));
end;

procedure TDatabasesList.Lock;
begin
  // if assigned(FLock) then
  // FLock.Acquire;
  System.TMonitor.Enter(self);
end;

procedure TDatabasesList.UnLock;
begin
  // if assigned(FLock) then
  // FLock.Release;
  System.TMonitor.exit(self);
end;

{ TFDParamHelper }

procedure ExchangeFieldType(qry: TFireQuery; fld: string; ftClass: TFieldClass);
var
  i: Integer;
  f, tmp: TField;
  n: string;
begin
  System.TMonitor.Enter(qry);
  try
    for i := 0 to qry.FieldCount - 1 do
    begin
      if sametext(qry.Fields[i].FullName, fld) then
      begin
        f := qry.Fields[i];
        n := f.Name;
        tmp := ftClass.Create(qry);
        tmp.FieldName := f.FieldName;
        f.Free;
        tmp.DataSet := qry;
        exit;
      end;
    end;
  finally
    System.TMonitor.exit(qry);
  end;
end;

{$IFDEF MSWINDOWS}

var
  LTrace: TFDMoniRemoteClientLink;
{$ENDIF}

constructor TFireDacDataModule.Create(ow: TComponent);
begin
  inherited Create(ow);
{$IFDEF MSWINDOWS}
  FDPhysFBDriverLink1 := TFDPhysFBDriverLink.Create(self);
{$ENDIF}
  // FDGUIxWaitCursor1:= TFDGUIxWaitCursor.create(self);
  // FDPhysSQLiteDriverLink1:= TFDPhysSQLiteDriverLink.create(self);
end;

{$IFDEF VCL}

procedure TFireDacDataModule.FDGUIxLoginDialog1Login(ASender: TObject; var AResult: Boolean);
var
  dlg: TFireDacLoginDlgBase;
begin
  dlg := TFireDacLoginDlgBase.Create(self);
  try
    dlg.ShowModal;
    if dlg.ModalResult = mrOk then
    begin
      AResult := true;
(*
{$IFDEF VER280}
      FDGUIxLoginDialog1.ConnectionDef.Params.UserName := dlg.User_Name.Text;
      FDGUIxLoginDialog1.ConnectionDef.Params.Password := dlg.Password.Text;
{$ELSE}
      FDGUIxLoginDialog1.  ConnectionDef.UserName := dlg.User_Name.Text;
      FDGUIxLoginDialog1.ConnectionDef.Password := dlg.Password.Text;
{$ENDIF}
*)
    end;
  finally
    dlg.Free;
  end;
end;
{$ENDIF}

function FireDacModule: TFireDacDataModule;
begin
  result := LFireDacDataModule;
end;

{ TFireScript }

procedure TFireScript.ExecSQL;
begin
  ExecuteScript(SQL);
end;

procedure TFireScripts.Clear;
begin
  inherited SQLScripts.Clear;
end;

function TFireScripts.ExecuteAll: Boolean;
begin
  result := false;
  if (SQLScripts.count = 0) and (SQLScriptFileName = '') then
    exit;
  result := inherited ExecuteAll;
end;

function TFireScripts.GetDatabase: TFireDatabase;
begin
  result := TFireDatabase(inherited Connection);

end;

function TFireScripts.GetDatabasename: string;
begin
  result := FDatabasename;
end;

function TFireScript.GetSQL: TStrings;
begin
  if inherited SQLScripts.count = 0 then
    inherited SQLScripts.Add;
  result := inherited SQLScripts[0].SQL;
end;

procedure TFireScripts.SetDatabase(const Value: TFireDatabase);
begin
  inherited Connection := Value;
  if assigned(Value) then
    FDatabasename := Value.Databasename;
end;

procedure TFireScripts.SetDatabasename(const Value: string);
var
  ss: TFireSession;
begin

  System.TMonitor.Enter(self);
  try
    // LLock.Acquire;

    FDatabasename := Value;

    if FDatabasename <> '' then
    begin
      // ss := FireSessions.FindSession(SessionName);
      // if not assigned(ss) then
      ss := FireSession;
      Database := ss.FindDataBase(Value);
      // ConnectionName := Value;

    end;

  finally
    // LLock.Release;
    System.TMonitor.exit(self);
  end;
end;

procedure TFireScript.SetSQL(const Value: TStrings);
begin
  if inherited SQLScripts.count = 0 then
    inherited SQLScripts.Add;
  inherited SQLScripts[0].Assign(Value);
end;

initialization
 ShowTables(nil); // dummy;
{$IFDEF MSWINDOWS}
if (fileExists(ExtractFilePath(ParamStr(0)) + 'debug.on')) or FindCmdLineSwitch('D', ['-', '\', '/'], true) then
begin
{$IFDEF LOG}
  debugLog('Iniciando monitor do FireDac');
{$ENDIF}
  LTrace := TFDMoniRemoteClientLink.Create(nil);
  //DebugLog('Arquivo Debug: '+ExtractFilePath(ParamStr(0)) + 'debug.on');
  with TIniFIle.Create(ExtractFilePath(ParamStr(0)) + 'debug.on') do
    try
      LTrace.Host := readString('Server','Host', 'localhost');
      LTrace.Port := ReadInteger('Server','Port',8050);
      LTrace.Tracing := LTrace.Port > 0;
    finally
      Free;
    end;

end;
{$ENDIF}
LFireDacDataModule := TFireDacDataModule.Create(nil);
LDatabases := TDatabasesList.Create;
LSessions := TFireSessions.Create(nil);
LSession := TFireSession.Create(nil);
LSessions.LoadConfig;

{$IFDEF MSWINDOWS}
// cria database local
LLocalConnection := TFireDatabase.Create(nil);
FillLocalSQLConnectionParams(LLocalConnection);

LLocalSQL := TFDLocalSQL.Create(nil);
LLocalSQL.Connection := LLocalConnection;

{$ENDIF}

finalization

LOnFinalization := true;

{$IFDEF MSWINDOWS}
FreeAndNil(LLocalSQL);
FreeAndNil(LLocalConnection);
{$ENDIF}
FreeAndNil(LSessions);
FreeAndNil(LSession);
FreeAndNil(LDatabases);
LDatabases := nil;
{$IFDEF MSWINDOWS}
FreeAndNil(LTrace);
{$ENDIF}

FreeAndNil(LFireDacDataModule);

LOnFinalizationED := true;

{$ENDIF}

end.
