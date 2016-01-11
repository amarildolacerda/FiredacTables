# FiredacTables


Code BDE style and access Firedac library.

First of all, need create some thing to change  BDE Config way - 
There are two possibilities:
1. Load from Firedac Config;
2. Create a new file with options to load on start fireTables;

Reference Table
<table>
<tr>
   <td> BDE</td>
   <td> FireTables </td>
</tr>
<tr>
  <td>TDatabase</td>
  <td>TFireDatabase</td>
</td>
<tr>
  <td>TSession</td>
  <td>TFireSession</td>
</td>
<tr>
  <td>TTable</td>
  <td>TFireTable</td>
</td>
<tr>
  <td>TSessions</td>
  <td>TFireSessions</td>
</td>
<tr>
  <td>TQuery</td>
  <td>TFireQuery</td>
</td>
<tr>
  <td>TStoredProc</td>
  <td>TFireStoredProc</td>
</td>
<tr>
  <td>Session</td>
  <td>FireSession</td>
</td>
<tr>
  <td>Sessions</td>
  <td>FireSessions</td>
</td>

<table>



To replace BDE completely, its possibile register packages BDERTL  (Tested with XE7 without BDE Package);

With BDE packages instaled, use compiler directive   "FIREDAC"  and alter uses do include  "bdeConnector";


