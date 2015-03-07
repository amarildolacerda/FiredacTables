unit fireTableReg;

interface

uses Classes,FireTables, DbTables, DesignIntf;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FireDacBDE',[TQuery,TTable,TStoredProc,TUpdateSql,TDatabase,TSession,TBatchMove]);
  UnlistPublishedProperty(TBatchMove,'TextFileName');
  UnlistPublishedProperty(TBatchMove,'TextDataDef');
  UnlistPublishedProperty(TBatchMove,'LogFileName');
  UnlistPublishedProperty(TBatchMove,'Mappings');
end;


end.
