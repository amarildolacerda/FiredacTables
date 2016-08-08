unit fireTablesEditor;

interface

uses
  SysUtils, Classes, DesignIntf,
  DesignEditors, DB,
  Data.FireTables, FireDAC.VCLUI.ConnEdit ;

type

  TFireAliasNameEditor = class(TStringProperty)
  private
    procedure GetValues(Proc: TGetStrProc); override;

  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TFireDatabasenameEditor = class(TStringProperty)
  private
    procedure GetValues(Proc: TGetStrProc); override;

  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TFireAliasNameReloadAliases = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TFireQueryView = class(TComponentEditor)
  public
    function NewIndex(index:integer):integer;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


procedure Register;

implementation

uses IniFiles, {$ifdef BPL}IniFilesEx, dbQueryView{$endif};

procedure Register;
begin

  RegisterPropertyEditor(TypeInfo(string), TFireDatabase, 'AliasName', TFireAliasNameEditor);

  RegisterPropertyEditor(TypeInfo(string), TFireQuery, 'Databasename', TFireDatabasenameEditor);
  RegisterPropertyEditor(TypeInfo(string), TFireTable, 'Databasename', TFireDatabasenameEditor);
  RegisterPropertyEditor(TypeInfo(string), TFireStoredProc, 'Databasename', TFireDatabasenameEditor);

  RegisterComponentEditor(TFireDatabase, TFireAliasNameReloadAliases);
//  RegisterComponentEditor(TFireQuery, TFireQueryView);
//  RegisterComponentEditor(TFireTable, TFireQueryView);

end;

{ TFireAliasNameEditor }

function TFireAliasNameEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList];
end;

procedure TFireAliasNameEditor.GetValues(Proc: TGetStrProc);
var
  FAliases: TStringList;
  s: string;
begin
  if FireSession <> nil then
  begin
    FAliases := TStringList.Create;
    try
      FireSession.GetAliasNames(FAliases);
      FAliases.Sort;
      for s in FAliases do
        Proc(s); // Beware, written in Browser!
    finally
      FAliases.Free;
    end;
  end;
end;

{ TFireAliasNameReloadAliases }

procedure TFireAliasNameReloadAliases.ExecuteVerb(Index: Integer);
begin
  // inherited;
  // TFireSessions(FireSessions).LoadConfig;
  case index of
    0: ;//TFireSessions(FireSessions).LoadConfig;
  end;
end;

function TFireAliasNameReloadAliases.GetVerb(Index: Integer): string;
begin
  case Index  of
   0: result := '&Recerregar';
//   1: result := '&Visualizar';
  end;
end;

function TFireAliasNameReloadAliases.GetVerbCount: Integer;
begin
  result := 1;
end;

{ TFireDatabasenameEditor }

function TFireDatabasenameEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList];
end;

procedure TFireDatabasenameEditor.GetValues(Proc: TGetStrProc);
var
  FAliases: TStringList;
  s: string;
  i: Integer;
begin
  if FireSession <> nil then
  begin
    FAliases := TStringList.Create;
    try
      FireSession.GetDatabaseNames(FAliases);
      for i := 0 to FireSession.DatabaseCount - 1 do
      begin
        s := FireSession.Databases[i].Databasename;
        if s<>'' then
        if FAliases.IndexOf(s) < 0 then
          FAliases.Add(s);
      end;
      FAliases.Sort;
      for s in FAliases do
        Proc(s); // Beware, written in Browser!
    finally
      FAliases.Free;
    end;
  end;
end;

{ TFireQueryView }

procedure TFireQueryView.ExecuteVerb(Index: Integer);
begin
  case NewIndex(Index) of
    -1: inherited ExecuteVerb(index);
    0: if Component is TDataset then
          DatasetView( Component as TDataset );
  end;
end;

function TFireQueryView.GetVerb(Index: Integer): string;
begin
   case newIndex(index) of
     -1 : result := inherited GetVerb(Index);
     0 : result := '&Visualizar';
   end;
end;

function TFireQueryView.GetVerbCount: Integer;
begin
   result := inherited GetVerbCount + 1;
end;

function TFireQueryView.NewIndex(index: integer): integer;
begin
  if Index < inherited GetVerbCount then
    Result := -1
  else begin
    result := Index - inherited GetVerbCount;
  end;
end;

end.
