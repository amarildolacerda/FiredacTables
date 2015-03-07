unit bdeConnector;

interface

{$I fireTables.inc}

uses Classes, SysUtils, {$ifdef FIREDAC} fireTables {$else} dbTables {$endif};


type
   TQuery = {$ifdef FIREDAC} fireTables.TFireQuery {$else} dbTables.TQuery {$endif};
   TTable = {$ifdef FIREDAC} fireTables.TFireTable {$else} dbTables.TTable {$endif};
   TStoredProc = {$ifdef FIREDAC} fireTables.TFireStoredProc {$else} dbTables.TStoredProc {$endif};
   TSession = {$ifdef FIREDAC} fireTables.TFireSession {$else} dbTables.TSession {$endif};
   TDatabase = {$ifdef FIREDAC} fireTables.TFireDatabase {$else} dbTables.TDatabase {$endif};



implementation

end.
