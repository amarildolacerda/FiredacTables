{ *************************************************************************** }
{ }
{ }
{ Copyright (C) Amarildo Lacerda }
{ }
{ https://github.com/amarildolacerda }
{ }
{ }
{ *************************************************************************** }
{ }
{ Licensed under the Apache License, Version 2.0 (the "License"); }
{ you may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at }
{ }
{ http://www.apache.org/licenses/LICENSE-2.0 }
{ }
{ Unless required by applicable law or agreed to in writing, software }
{ distributed under the License is distributed on an "AS IS" BASIS, }
{ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{ See the License for the specific language governing permissions and }
{ limitations under the License. }
{ }
{ *************************************************************************** }

unit BDE.DBTables;

interface

uses Classes, sysUtils, DB, Data.FireTables, BDE {,FireDAC.Stan.Error};

{ .$ifdef VER280 }
{.$IFDEF BDE2FIREDAC}

const
  dbfDatabase: Integer = 9;
  dbfExecProc: Integer = 7;
  dbfExecSQL: Integer = 2;
  dbfFieldList: Integer = 4;
  dbfIndexList: Integer = 5;
  dbfOpened: Integer = 0;
  dbfPrepared: Integer = 1;
  dbfProcDesc: Integer = 8;
  dbfProvider: Integer = $A;
  dbfStoredProc: Integer = 6;
  dbfTable: Integer = 3;
  smTraceBufSize: Integer = $8006;

Type

  EDBEngineError = class(Exception)
  public
  end; // EFDDBEngineException;

  ENoResultSet = class(EDatabaseError);

  HDBISES = THandle;
  HDBIDB = THandle;
  TLocale = ^Pointer;

  TTraceFlag = (tfQPrepare, tfQExecute, tfError, tfStmt, tfConnect, tfTransact,
    tfBlob, tfMisc, tfVendor, tfDataIn, tfDataOut);

  TTraceFlags = set of TTraceFlag;

  TTransIsolation = (tiDirtyRead, tiReadCommitted, tiRepeatableRead);

  TRecNoStatus = (rnDbase, rnParadox, rnNotSupported);

  TBatchMode = (batAppend, batUpdate, batAppendUpdate, batDelete, batCopy);

  TBlobStream = class(TStream)
  public
    constructor create(fb: TBlobField; mode: TBlobStreamMode);
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;

  // TLocale = class(TObject);
  TUpdateSql = class(TFireUpdateSql)
  public
  end;

  TDatabase = class(TFireDatabase)
  private
    FHandle: HDBIDB;
    FLocale: TLocale;
    FSessionAlias: Boolean;
    FHandleShared: Boolean;
    function GetDirectory: string;
    procedure SetDirectory(const Value: string);
    procedure SetHandle(const Value: HDBIDB);
    function GetTraceFlags: TTraceFlags;
    procedure SetTraceFlags(const Value: TTraceFlags);
    function GetParams: TStrings;
    procedure SetParams(const Value: TStrings);
  PUBLIC
    procedure FlushSchemaCache(const TableName: string);
    procedure ValidateName(const Name: string);
    property Directory: string read GetDirectory write SetDirectory;
    property Handle: HDBIDB read FHandle write SetHandle;
    property Locale: TLocale read FLocale;
    property SessionAlias: Boolean read FSessionAlias;
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property HandleShared: Boolean read FHandleShared write FHandleShared
      default False;
    property Params: TStrings read GetParams write SetParams;
  published
    property Connected;
  END;

  TDatabaseLoginEvent = procedure(Database: TDatabase; LoginParams: TStrings)
    of object;

  TParamHelper = class helper for TFireParam
  public
    procedure SetBlobData(Buffer: TValueBuffer; Size: Integer);
  end;

  TParamsHelper = class helper for TFireParams
  public
    procedure AddParam(Value: TParam);
    procedure RemoveParam(Value: TParam);

  end;

  TSession = class;
  TBDEDataset = class;

  hDBIStmt = Pointer;

  TQuery = class(TFireQuery)
  private
    FLocal: Boolean;
    FStmtHandle: hDBIStmt;
{$ifdef MSWINDOWS}
    FSQLBinary: PAnsiChar;
{$endif}
    FConstrained: Boolean;
  public
    property Local: Boolean read FLocal;
    property StmtHandle: hDBIStmt read FStmtHandle write FStmtHandle;
{$ifdef MSWINDOWS}
    property SQLBinary: PAnsiChar read FSQLBinary write FSQLBinary;
{$endif}
    property Constrained: Boolean read FConstrained write FConstrained
      default False;
  published
    property Active;
  end;

  TIndexOption = (ixPrimary, ixUnique, ixDescending, ixCaseInsensitive,
    ixExpression);
  TIndexOptions = set of TIndexOption;
  TTableType = (ttDefault, ttParadox, ttDBase, ttFoxPro, ttASCII);

  TLockType = (ltReadLock, ltWriteLock);

  TTable = class(TFireTable)
  private
    FTableLevel: Integer;
    FIndexFiles: TStrings;
    function GetIndexField(Index: Integer): TField;
    procedure SetIndexField(Index: Integer; const Value: TField);
    function GetTableLevel: Integer;
    procedure SetIndexFiles(const Value: TStrings);
  public
    function BatchMove(ASource: TBDEDataset; AMode: TBatchMode): Longint;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions;
      const DescFields: string = '');
    procedure CloseIndexFile(const IndexFileName: string);
    procedure DeleteTable;
    procedure LockTable(LockType: TLockType);
    procedure OpenIndexFile(const IndexName: string);
    procedure RenameTable(const NewTableName: string);
    procedure UnlockTable(LockType: TLockType);
    property IndexFields[Index: Integer]: TField read GetIndexField
      write SetIndexField;
    property TableLevel: Integer read GetTableLevel write FTableLevel;
    property IndexFiles: TStrings read FIndexFiles write SetIndexFiles;
  published
    property Active;
  end;

  DBIResult = Integer;

  TDBDataSet = class(TFireQuery)
  private
    function GetDBHandle: HDBIDB;
    function GetDBLocale: TLocale;
    function GetDBSession: TSession;
  protected
    procedure CloseCursor; virtual;
  public
    function CheckOpen(Status: DBIResult): Boolean;
    procedure CloseDatabase(Database: TDatabase);
    function OpenDatabase: TDatabase;
    property DBHandle: HDBIDB read GetDBHandle;
    property DBLocale: TLocale read GetDBLocale;
    property DBSession: TSession read GetDBSession;
    function CreateHandle: HDBICur; virtual;
  end;

  TDataSetUpdateObject = class(TFireUpdateSql);
  TUpdateRecordTypes = (rtModified, rtInserted, rtDeleted, rtUnmodified);

  TBDEDataset = class(TDBDataSet)
  private
    FCacheBlobs: Boolean;
    FExpIndex: Boolean;
    FHandle: HDBICur;
    FKeySize: Word;
    FLocale: TLocale;
    function GetUpdateRecordSet: TUpdateRecordTypes;
    procedure SetUpdateRecordSet(const Value: TUpdateRecordTypes);
  public
    property CacheBlobs: Boolean read FCacheBlobs write FCacheBlobs
      default True;
    function ConstraintsDisabled: Boolean;
    procedure FlushBuffers;
    procedure GetIndexInfo;
    property ExpIndex: Boolean read FExpIndex;
    property Handle: HDBICur read FHandle;
    property KeySize: Word read FKeySize;
    property Locale: TLocale read FLocale;
    property UpdateRecordTypes: TUpdateRecordTypes read GetUpdateRecordSet
      write SetUpdateRecordSet;
  end;

  TConfigMode = (cfmVirtual, cfmPersistent, cfmSession);
  TDatabaseEvent = (dbOpen, dbClose, dbAdd, dbRemove, dbAddAlias, dbDeleteAlias,
    dbAddDriver, dbDeleteDriver);

  TSession = class(TFireSession)
  private
    FLocale: TLocale;
    FTraceFlags: TTraceFlags;
    FAutoSessionName: Boolean;
    FKeepConnections: Boolean;
    FSQLHourGlass: Boolean;
    function GetConfigMode: TConfigMode;
    procedure SetConfigMode(const Value: TConfigMode);
    function GetHandle: HDBISES;
    procedure SetTraceFlags(const Value: TTraceFlags);
    procedure SetAutoSessionName(const Value: Boolean);
    function GetPrivateDir: String;
    procedure SetPrivateDir(const Value: String);
  public
    property ConfigMode: TConfigMode read GetConfigMode write SetConfigMode;
    procedure AddPassword(const Password: string);
    procedure DeleteDriver(const Name: string);
    procedure DropConnections;
    procedure GetConfigParams(const Path, Section: string; List: TStrings);
    procedure GetDriverNames(List: TStrings);
    procedure GetDriverParams(const DriverName: string; List: TStrings);
    function GetPassword: Boolean;
    procedure GetStoredProcNames(const DatabaseName: string; List: TStrings);
    procedure ModifyDriver(Name: string; List: TStrings);
    procedure RemoveAllPasswords;
    procedure RemovePassword(const Password: string);
    property Handle: HDBISES read GetHandle;
    property Locale: TLocale read FLocale;
    property TraceFlags: TTraceFlags read FTraceFlags write SetTraceFlags;
    property AutoSessionName: Boolean read FAutoSessionName
      write SetAutoSessionName default False;
    property KeepConnections: Boolean read FKeepConnections
      write FKeepConnections default True;
    property PrivateDir: String read GetPrivateDir write SetPrivateDir;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass
      default True;
  end;

  TParamBindMode = (pbByName, pbByNumber);

  TStoredProc = class(TFireStoredProc)
  private
    FStmtHandle: hDBIStmt;
    FBindMode: TParamBindMode;
    function GetParams: TParams;
    procedure SetParams(const Value: TParams);
  public
    procedure CopyParams(Value: TParams);
    property StmtHandle: hDBIStmt read FStmtHandle write FStmtHandle;
    property Params: TParams read GetParams Write SetParams;
    property ParamBindMode: TParamBindMode read FBindMode write FBindMode
      default pbByName;
  end;
{$ifdef MSWINDOWS}
  TBatchMove = class(TFireBatchMove)
  private
    FChangedCount: Longint;
    FKeyViolCount: Longint;
    FMovedCount: Longint;
    FProblemCount: Longint;
    FAbortOnKeyViol: Boolean;
    FAbortOnProblem: Boolean;
    FChangedTableName: TFileName;
    FKeyViolTableName: TFileName;
    FProblemTableName: TFileName;
    FTransliterate: Boolean;
    function GetMappings: TStrings;
    procedure SetMappings(const Value: TStrings);
  public
    property ChangedCount: Longint read FChangedCount;
    property KeyViolCount: Longint read FKeyViolCount;
    property MovedCount: Longint read FMovedCount;
    property ProblemCount: Longint read FProblemCount;
    property AbortOnKeyViol: Boolean read FAbortOnKeyViol write FAbortOnKeyViol
      default True;
    property AbortOnProblem: Boolean read FAbortOnProblem write FAbortOnProblem
      default True;
    property ChangedTableName: TFileName read FChangedTableName
      write FChangedTableName;
    property KeyViolTableName: TFileName read FKeyViolTableName
      write FKeyViolTableName;
    property Mappings: TStrings read GetMappings write SetMappings;
    property ProblemTableName: TFileName read FProblemTableName
      write FProblemTableName;
    property Transliterate: Boolean read FTransliterate write FTransliterate
      default True;
  end;
{$endif}

  TSessions = class(TFireSessions)
  private
    function GetList(nome: string): TSession;
    procedure SetList(nome: string; const Value: TSession);
  public
    function FindSession(sSession: String): TSession;
    property List[nome: string]: TSession read GetList write SetList;
  end;

function Session: TSession;
function Sessions: TSessions;
{ .$ENDIF }
{ .$endif }

implementation

{ .$ifdef VER280 }
{ .$IFDEF BDE2FIREDAC }
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
{ .$ENDIF }
{ .$endif }

{ TDBDataSet }

function TDBDataSet.CheckOpen(Status: DBIResult): Boolean;
begin
  result := Active;
end;

procedure TDBDataSet.CloseCursor;
begin

end;

procedure TDBDataSet.CloseDatabase(Database: TDatabase);
begin

end;

function TDBDataSet.CreateHandle: HDBICur;
begin

end;

function TDBDataSet.GetDBHandle: HDBIDB;
begin

end;

function TDBDataSet.GetDBLocale: TLocale;
begin

end;

function TDBDataSet.GetDBSession: TSession;
begin

end;

function TDBDataSet.OpenDatabase: TDatabase;
begin

end;

{ TSession }

procedure TSession.AddPassword(const Password: string);
begin

end;

procedure TSession.DeleteDriver(const Name: string);
begin

end;

procedure TSession.DropConnections;
begin

end;

function TSession.GetConfigMode: TConfigMode;
begin

end;

procedure TSession.GetConfigParams(const Path, Section: string; List: TStrings);
begin

end;

procedure TSession.GetDriverNames(List: TStrings);
begin

end;

procedure TSession.GetDriverParams(const DriverName: string; List: TStrings);
begin

end;

function TSession.GetHandle: HDBISES;
begin

end;

function TSession.GetPassword: Boolean;
begin
  result := True;
end;

function TSession.GetPrivateDir: String;
begin

end;

procedure TSession.GetStoredProcNames(const DatabaseName: string;
  List: TStrings);
begin

end;

procedure TSession.ModifyDriver(Name: string; List: TStrings);
begin

end;

procedure TSession.RemoveAllPasswords;
begin

end;

procedure TSession.RemovePassword(const Password: string);
begin

end;

procedure TSession.SetAutoSessionName(const Value: Boolean);
begin
  FAutoSessionName := Value;
end;

procedure TSession.SetConfigMode(const Value: TConfigMode);
begin

end;

procedure TSession.SetPrivateDir(const Value: String);
begin

end;

procedure TSession.SetTraceFlags(const Value: TTraceFlags);
begin
  FTraceFlags := Value;
end;

{ TDatabase }

procedure TDatabase.FlushSchemaCache(const TableName: string);
begin

end;

function TDatabase.GetDirectory: string;
begin

end;

function TDatabase.GetParams: TStrings;
begin
  result := TStrings(inherited Params);
end;

function TDatabase.GetTraceFlags: TTraceFlags;
begin

end;

procedure TDatabase.SetDirectory(const Value: string);
begin

end;

procedure TDatabase.SetHandle(const Value: HDBIDB);
begin
  FHandle := Value;
end;

procedure TDatabase.SetParams(const Value: TStrings);
begin
  inherited Params.Assign(Value);
end;

procedure TDatabase.SetTraceFlags(const Value: TTraceFlags);
begin

end;

procedure TDatabase.ValidateName(const Name: string);
begin

end;

{ TBDEDataset }

function TBDEDataset.ConstraintsDisabled: Boolean;
begin

end;

procedure TBDEDataset.FlushBuffers;
begin

end;

procedure TBDEDataset.GetIndexInfo;
begin

end;

function TBDEDataset.GetUpdateRecordSet: TUpdateRecordTypes;
begin

end;

procedure TBDEDataset.SetUpdateRecordSet(const Value: TUpdateRecordTypes);
begin

end;

{ TTable }

procedure TTable.AddIndex(const Name, Fields: string; Options: TIndexOptions;
  const DescFields: string);
begin

end;

function TTable.BatchMove(ASource: TBDEDataset; AMode: TBatchMode): Longint;
begin

end;

procedure TTable.CloseIndexFile(const IndexFileName: string);
begin

end;

procedure TTable.DeleteTable;
begin

end;

function TTable.GetIndexField(Index: Integer): TField;
begin

end;

function TTable.GetTableLevel: Integer;
begin
  result := FTableLevel;
end;

procedure TTable.LockTable(LockType: TLockType);
begin

end;

procedure TTable.OpenIndexFile(const IndexName: string);
begin

end;

procedure TTable.RenameTable(const NewTableName: string);
begin

end;

procedure TTable.SetIndexField(Index: Integer; const Value: TField);
begin

end;

procedure TTable.SetIndexFiles(const Value: TStrings);
begin
  FIndexFiles := Value;
end;

procedure TTable.UnlockTable(LockType: TLockType);
begin

end;

{ TBatchMove }
{$ifdef MSWINDOWS}
function TBatchMove.GetMappings: TStrings;
begin
  result := TStrings(inherited Mappings);
end;

procedure TBatchMove.SetMappings(const Value: TStrings);
begin
  inherited Mappings.Assign(Value);
end;
{$endif}

{ TStoredProc }

procedure TStoredProc.CopyParams(Value: TParams);
begin
  inherited Params.Assign(Value);
end;

function TStoredProc.GetParams: TParams;
begin
  result := TParams(Inherited Params);
end;

procedure TStoredProc.SetParams(const Value: TParams);
begin
  inherited Params.Assign(Value);
end;

{ TParam }

procedure TParamHelper.SetBlobData(Buffer: TValueBuffer; Size: Integer);
begin

end;

{ TParams }

procedure TParamsHelper.AddParam(Value: TParam);
var
  fp: TFireParam;
begin
  fp := add;
  fp.Assign(Value);
end;

procedure TParamsHelper.RemoveParam(Value: TParam);
var
  I: Integer;
begin
  for I := 0 to count - 1 do
  begin
    if sametext(Value.Name, items[I].Name) then
    begin
      delete(I);
      exit;
    end;
  end;
end;

{ TBlobStream }

constructor TBlobStream.create(fb: TBlobField; mode: TBlobStreamMode);
begin
  inherited create;
end;

function TBlobStream.Seek(Offset: Integer; Origin: Word): Longint;
begin

end;

procedure TBlobStream.Truncate;
begin

end;

{ EDBEngineError }

function TSessions.GetList(nome: string): TSession;
begin
  result := TSession(inherited List[nome]);
end;

procedure TSessions.SetList(nome: string; const Value: TSession);
begin
  inherited List[nome] := Value;
end;

end.

