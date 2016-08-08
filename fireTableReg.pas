unit fireTableReg;

interface

uses System.Classes, Data.FireTables, BDE.DbTables, DesignIntf;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FireDacBDE',[TQuery,TTable,TStoredProc,TUpdateSql,TDatabase,TSession,TBatchMove]);
  UnlistPublishedProperty(TQuery,'MacroCheck');
  UnlistPublishedProperty(TDatabase,'ConnectionName');
  UnlistPublishedProperty(TDatabase,'ConnectionDefName');
  UnlistPublishedProperty(TBatchMove,'TextFileName');
  UnlistPublishedProperty(TBatchMove,'TextDataDef');
  UnlistPublishedProperty(TBatchMove,'LogFileName');
  UnlistPublishedProperty(TBatchMove,'Mappings');
end;


end.
