{ 31/05/2005 09:57:32 (GMT-3:00) > [amarildo on FBI] checked in   }
{ 31/05/2005 09:43:26 (GMT-3:00) > [amarildo on FBI] checked out / }
unit FiscalBase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PerifHdr, ExtCtrls, {dbTables,} IniFiles,iniFilesEx{, FiscalBaseExports};

  // fiscalBaseExports
type


{  TDadosSintegra = record
    cRegistros,
      cArquivoMFD,
      cArquivo,
      cMes,
      cAno,
      cRazaoSocial,
      cEndereco,
      cNumero,
      cComplemento,
      cBairro,
      cCidade,
      cCEP,
      cTelefone,
      cFax,
      cContato: string;
  end;
}



  TFiscalBase = class(TImpCupomAbstComm32)
  private
    FTabelaAliquotaList:TStringList;
    FFormaPgtoList:TStringList;
    FFechaCupomMsg: TStringList;
    FDataBaseName: string;
    FDadosSintegra: TDadosSintegra;
    function Configurar(op: Shortstring): boolean; override;
    //procedure SetMapaResumoItems(const Value: TMapaResumoItems);
    procedure SetDataBaseName(const Value: string);
    procedure SetDadosSintegra(const Value: TDadosSintegra);
  protected
    QtdeDeItensImpressos: Integer;
    FInverteChecaGavetaAberta:Boolean;

 //   procedure Login;
  public

    FEmiteLeiturXPosTrocaBobina:boolean;

  {$REGION 'CNFNV'}
    function EmiteCNFNV(AValor: Extended; AIndice: string;
      ATipoPgto: string = ''): Boolean; override;
{$ENDREGION}
    property DadosSintegra: TDadosSintegra read FDadosSintegra write SetDadosSintegra;

    function GetUltimaALiquotaUtilizada: string;virtual;
    function FormaPgtoMFD(str:string):string;virtual;
    function TabelaAliquotaMFD(str:string):string;virtual;
    property TabelaAliquotaList:TSTringList read FTabelaAliquotaList;
    property FormaPgtoList:TStringList read FFormaPgtoList;
    function FechaCupomMsg(msg_: string): TStringList;
    function PegaRegistradores: boolean; override;
    function CancVenda(op, NrCupom: ShortString;
      vTotal: Extended): boolean; override;
    function CancCupom(op, NrCupom: ShortString;
      vTotal: Extended): boolean; override;
    function SangriaCx(msg: ShortString; Valor: Extended;
      TipoPgto: string = ''): boolean; override;
    function SuprimentoCx(msg: ShortString; Valor: Extended;
      TipoPgto: string = ''): boolean; override;
    property DataBaseName: string read FDataBaseName write SetDataBaseName;
    procedure close; override;
    function LeMapaResumo(reducao: Integer = 0): Boolean; virtual;
    function RegistraMapaResumo: Boolean; override;

    constructor Create(starting: Boolean); override;
    destructor Destroy; override;
    function cancelaDadosCupom(cup: string): Boolean;

    function ImprimeItemDescTotal(codProd, Unid, Descr,
      CodTrib: ShortString; Qt, VUnit, VDesc, vAcres, ATrib,
      Reduc: Extended): boolean; virtual;
    function GetSubTotal: CURRENCY; virtual;

    function GetNewDBDcto(exp: string; recarregar:boolean=false): double; virtual;
    function GeraNumeroCupomFiscal: Boolean; override;
    procedure SetCooInternal(dcto: string);
    procedure log(s: string);

    function NecessarioTrocaBobina: Boolean; override;
    function TrocaBobinaPapel: Boolean; override;
  end;


implementation

uses {$IFNDEF IFARQUIVO}wDlgSetupFiscalAbst,{$ENDIF}dllFunctions;

function TFiscalBase.cancelaDadosCupom(cup: string): Boolean;
  //var
//  qry: TALQuery;
//  db: TALQUery;
//    s: string;
  begin
  Result := False;
  // o cancelamento no banco de dados esta sendo efetuado dentro da aplicacao.
  try
    if assigned(FAfterCancelaCupomEvent) then
      FAfterCancelaCupomEvent(self, cup, PrinterSerie, cup = NumeroCupom);
  except
    on e: Exception do
    begin
      OkDlg(e.Message);
      Result := False;
    end;
  end;
(*
  Login;

{  if assigned(query) then
    qry := TAlQuery(query)
  else}
  qry := TALQuery.create(nil);

  try
    db := tAlQuery.create(nil);
    try
      qry.databasename := DataBaseName;
      qry.sql.text := 'select * from sigcaut2 where (dcto=:dcto) and (prtserie=:prtserie)';
      qry.ParamByName('dcto').asString := padl(cup, 10, '0');
      qry.ParamByName('prtserie').asString := PrinterSerie;
      qry.open;
      db.DataBaseName := dataBaseName;
      s := createInsertSql('sigcaut2', qry);
      while qry.eof = false do
        begin
          db.sql.text := s;
          PegaParametrosDe(db, qry);
{$IFDEF PDVTokStok}
          db.ParamByName('operacao').asString := '231';
          db.ParamByName('ordem').asInteger := 1000 + qry.FieldByName('ordem').asInteger;
{$ELSE}
          db.ParamByName('qtde').asFloat := -qry.FieldByName('qtde').asFloat;
{$ENDIF}
          if db.ParamByName('vendedor').isnull then
            db.ParamByName('vendedor').asString := '';
          if qry.FieldByName('qtde').asFloat <> 0 then
            db.ExecSql;
          qry.next;
        end;

      qry.close;
      qry.sql.text := 'select * from sig02 where (dcto=:dcto) and (prtserie=:prtserie)';
      qry.ParamByName('dcto').asString := padl(cup, 10, '0');
      qry.ParamByName('prtserie').asString := PrinterSerie;
      qry.open;
      s := createInsertSql('sig02', qry);
      while qry.eof = false do
        begin
          db.sql.text := s;
          PegaParametrosDe(db, qry);
{$IFDEF PDVTokStok}
          db.ParamByName('codigo').asString := '231';
{$ELSE}
          db.ParamByName('valor').asFloat := -qry.FieldByName('valor').asFloat;
{$ENDIF}
          AjustaNullField(db);
          if qry.FieldByName('VALOR').asFloat <> 0 then
            db.ExecSql;
          qry.next;
        end;

      result := true;
    finally
      db.free;
    end;
  finally
    if qry <> query then
      qry.free;
    try
      DisConnectDataBase(databaseName);
    except
    end;
  end;

 *)
  end;

function TFiscalBase.Configurar(op: Shortstring): boolean;
  begin
  {$IFNDEF IFARQUIVO}
    with TIFiscalConfig.create(nil) do
      try
        testeMenu.visible := isDebug;
        ShowDialog(self);
        Result := True;
        BringToFront;
      finally
        free;
      end;
  {$ENDIF}
  end;


const
  ini_trib_section = 'Manutencao_Tributacao';
  ini_forma_pgto   = 'Manutencao_Forma_Pgto';


constructor TFiscalBase.create(starting: Boolean);
  var i:integer;
  begin
    inherited create(starting);
    FEmiteLeiturXPosTrocaBobina:=true;
    FTabelaAliquotaList := tStringList.create;
    FFormaPgtoList := TStringList.create;


    with TIniFIle.create('pdv.ini') do
      try
        ReadSectionValues(ini_trib_section, fTabelaAliquotaList);
        ReadSectionValues(ini_forma_pgto, FFormaPgtoList);

        if FFormaPgtoList.count = 0 then
          with FFormaPgtoList do
          begin
            add('01=01');
            add('02=02');
            add('03=03');
            add('04=04');
            add('05=05');
            add('06=06');
            add('07=07');
            add('08=08');
            add('08=08');

            for i := 0 to count - 1 do
              writeString(ini_forma_pgto, Names[i], Values[Names[i]]);

          end;



        if fTabelaAliquotaList.count = 0 then
          with fTabelaAliquotaList do
          begin
            add('TA_=01');
            add('TB_=02');
            add('TC_=03');
            add('TD_=04');
            add('TE_=05');
            add('TF_=06');
            add('TG_=07');
            add('TH_=08');
            add('TI_=09');
            add('I=I');
            add('F=F');
            add('N=N');
            add('S=S');
            for i := 0 to count - 1 do
              writeString(ini_trib_section, Names[i], Values[Names[i]]);
          end;

      finally
        free;
      end;


    with TIniFile.create('pdv.ini') do
      try
        FilialCorrente := readInteger('Login', 'Filial', 0);
        FInverteChecaGavetaAberta := false;
        if readString('GAVETA','InverteChecaGavetaAberta','')='' then
          WriteString('GAVETA','InverteChecaGavetaAberta','NAO');
        FInverteChecaGavetaAberta:=readString('GAVETA','InverteChecaGavetaAberta','NAO')='SIM';
        IsGaveta :=
          (ReadString('GAVETA', 'Gaveta Instalada', 'NAO') = 'SIM')
          and
          (ReadString('GAVETA', 'Conectada à Impressora', 'NAO') = 'SIM')
      finally
        free;
      end;
    debug := fileExists('debug.on');
    FFechaCupomMsg := TStringList.create;
    Log('Inicializando o drive da impressora fiscal');
  end;

destructor TFiscalBase.destroy;
  begin
    FTabelaAliquotaList.free;
    FFormaPgtoList.free;

    FFechaCupomMsg.free;
    log('Encerrando o drive da impressora fiscal');
    inherited destroy;
  end;




function TFiscalBase.EmiteCNFNV(AValor: Extended; AIndice,
  ATipoPgto: string): Boolean;
  begin
  // Não coloque código aqui, declare a função na DLL como override.
  Result := False;
  end;

function TFiscalBase.GeraNumeroCupomFiscal: Boolean;
  var
    exp: string;
    i: Currency;
  begin
    FCNFInternal := '';
    exp := 'IF' + {Loja} padl(intToStr(filialCorrente), 4, '0') + ECF;


    if assigned(OnGetCtrlID) then OnGetCtrlID(self, exp, i);

    FCOOInternal := intToStr(trunc(i));
    FCNFInternal := FCOOInternal;
    result := true;
  end;



function TFiscalBase.GetNewDBDcto(exp: string; recarregar:boolean=false): double;
  var
    vRetorno: Currency;
  begin
    vRetorno := 0;
    if assigned(onGetCtrlId) then
      OnGetCtrlID(self, Exp, vRetorno);
    result := vRetorno;
  {  if pos('.DLL', UpperCase(ParamStr(0))) = 0 then
    if Assigned(query) then
      result := TALQuery(query).getId(Exp)
    else
      result := qst_financeiro.GetnewdbIDNoFilial('PontoVenda', exp, 1);}
  end;

function TFiscalBase.GetSubTotal: CURRENCY;
  begin
    result := 0;
  end;

function TFiscalBase.ImprimeItemDescTotal(codProd, Unid, Descr,
  CodTrib: ShortString; Qt, VUnit, VDesc, vAcres, ATrib,
  Reduc: Extended): boolean;
  begin // compatibilidade com biblioteca anterior- na IBM o desconto é pelo total.
    if qt <> 0 then begin
      if vAcres <> 0 then
        vAcres := roundFloat(vAcres / qt, 2);
      if vDesc <> 0 then
        vDesc := roundFloat(vDesc / qt, 2);
    end;



    result :=
      ImprimeItem(codProd, Unid, Descr,
      CodTrib, Qt, VUnit, VDesc, vAcres, ATrib,
      Reduc);

  end;

function TFiscalBase.TabelaAliquotaMFD(str: string): string;
  var x:string;
  begin
    result := str;
    x := FTabelaAliquotaList.values[str];
    if x<>'' then
      result := x;
  end;

function TFiscalBase.LeMapaResumo(reducao: Integer): Boolean;
  begin
    Result := False;
  end;



procedure TFiscalBase.log(s: string);
  begin
   // if debug then   -- eh chamado quando ocorre erro.
      WriteLog(format('IFErros_%s.log', [ddmmyyyy(date)]), s);
  end;



function TFiscalBase.NecessarioTrocaBobina: Boolean;
  begin
    result := false;
  end;

function TFiscalBase.RegistraMapaResumo: Boolean;
  begin
    if assigned(OnRegistraMapaResumo) then
      OnRegistraMapaResumo(self, result)
    else result := true;  
  end;

procedure TFiscalBase.SetCooInternal(dcto: string);
  begin
    fCooInternal := dcto;
  end;

procedure TFiscalBase.SetDadosSintegra(const Value: TDadosSintegra);
  begin
    FDadosSintegra := Value;
  end;

procedure TFiscalBase.SetDataBaseName(const Value: string);
  begin
    FDataBaseName := Value;
  end;


//procedure TFiscalBase.SetMapaResumoItems(const Value: TMapaResumoItems);
//  begin
//
//  end;

function TFiscalBase.GetUltimaALiquotaUtilizada: string;
  begin
    result := '' ;  // nao ha implementação na DLL para esta funçãop... retornar vazio.
   // em um mudança de estrutura das DLLs... pode passar esta funcção para a classe base.
   // enquanto isto, matem compatiblidade.
  end;


function TFiscalBase.TrocaBobinaPapel: Boolean;
  var
    op: boolean;
  begin


    if FEmiteLeiturXPosTrocaBobina then  // as impressoras terminas não precisam emitir X para iniciar a bobina.
    begin
      if simnao('Sensor de pouco/fim Papel acionado. Imprimir leitura X antes da troca da bobina ?')='N' then
      begin
        result := true;
        exit;
      end;

   // emite uma leitura X
      result := false;
      try
        if NecessarioTrocaBobina then
          LeituraX('Papel', now);
      except
      end;
      while true do
      begin
        op := true;

        if NecessarioTrocaBobina then
          op := SimNao('Retirar a bobina preenchida e substituir pela bobina nova.'#13#13 +
            'Troca concluída.') = 'S';

        if op and (NecessarioTrocaBobina = false) then
        begin
          LeituraX('Papel', now);
          result := true;
          exit;
        end;
        if SimNao('Detectado falta ou fim de papel.'#13#13 + 'Continuar o procedimento de troca de bobina ?') = 'N' then exit;
      end;
    end
    else
    begin
       OkDlg('Detectado falta ou fim de papel.');
       result := true;
    end;
  end;

function TFiscalBase.SuprimentoCx(msg: ShortString; Valor: Extended; TipoPgto: string = ''): boolean;
  var
    st: TstringList;
  begin
    st := TStringList.create;
    try
      st.add(pad('=', 40, '='));
      st.add('');
    //st.add('Suprimento de Caixa - Via 1');
      st.add('Suprimento de Caixa');
      st.add('Data: ' + datetimeToStr(now) + ' Cx: ' + Operador);
      st.add(msg);
      st.add('Valor: ' + FormatFloat('0.00', valor));
      st.add('');
      st.add('');
    //Tarefa 9367 - Calixto - 02/03/2009
    //st.add(pad('-', 40, '-'));
      st.add(pad('=', 40, '='));
      st.add('');
    {st.add('');
    st.add('');
    st.add('Suprimento de Caixa - Via 2');
    st.add('Data: ' + datetimeToStr(now) + ' Cx: ' + Operador);
    st.add(msg);
    st.add('Valor: ' + FormatFloat('0.00', valor));
    st.add('');
    st.add(pad('=', 40, '='));
    st.add('');}
      CupomNFiscal(st);
    finally
      st.free;
    end;
    result := true;
  end;

function TFiscalBase.SangriaCx(msg: ShortString; Valor: Extended; TipoPgto: string = ''): boolean;
  var
    st: TstringList;
  begin
    st := TStringList.create;
    try
      st.add(pad('=', 40, '='));
      st.add('');
    //Tarefa 9367 - Calixto - 02/03/2009
    //st.add('Sangria de Caixa - Via 1');
      st.add('Sangria de Caixa');
      st.add('Data: ' + datetimeToStr(now) + ' Cx: ' + Operador);
      st.add(msg);
      st.add('Valor: ' + FormatFloat('0.00', valor));
      st.add('');
      st.add('');
    //st.add(pad('-', 40, '-'));
      st.add(pad('=', 40, '='));
      st.add('');
    {st.add('');
    st.add('');
    st.add('Sangria de Caixa - Via 2');
    st.add('Data: ' + datetimeToStr(now) + ' Cx: ' + Operador);
    st.add(msg);
    st.add('Valor: ' + FormatFloat('0.00', valor));
    st.add('');
    st.add(pad('=', 40, '='));
    st.add('');}
      CupomNFiscal(st);
    finally
      st.free;
    end;
    result := true;
  end;





function TFiscalBase.PegaRegistradores: boolean;
  begin
    result := leMapaResumo(0);
  end;

function TFiscalBase.CancVenda(op, NrCupom: ShortString;
  vTotal: Extended): boolean;
  begin
{  result := false;
  if Assigned(AfterCancelaCupomEvent) then
    AfterCancelaCupomEvent(self, numerocupom, PrinterSerie, false);
  result := true;}
    result := CancCupom(op, nrCupom, vTotal);
  end;

procedure TFiscalBase.close;
  begin
    inherited;
  end;

function TFiscalBase.CancCupom(op, NrCupom: ShortString;
  vTotal: Extended): boolean;
  begin
    if Assigned(AfterCancelaCupomEvent) then
      AfterCancelaCupomEvent(self, numerocupom, PrinterSerie, false);
    result := true;
  end;

function TFiscalBase.FechaCupomMsg(msg_: string): TStringList;
  var msg: string;
  begin
    fFechaCupomMsg.Clear;
    msg := msg_;

    FFechaCupomMsg.add('MD5: '+IdentMD5Aplicativo);
    if msg <> '' then
      fFechaCupomMsg.add(Pad(msg, 40, ' '));

//  if (Operador <> '') or (Caixa <> '') then
//    fFechaCupomMsg.add('Op: ' + Operador + ' Caixa: ' + Caixa);

    if (Cliente.Nome <> '') and (Pos(Cliente.Nome, msg) = 0) then
      fFechaCupomMsg.add(Pad('Cli:' + Cliente.Nome, 40, ' '));

    if (Cliente.Ender <> '') and (Pos(Cliente.Ender, msg) = 0) then
      fFechaCupomMsg.Add(Pad('End:' + Cliente.Ender, 40, ' '));

    if (Cliente.Bairro <> '') and (Pos(Cliente.Bairro, msg) = 0) then
      fFechaCupomMsg.Add(Pad(Cliente.Bairro + '-' + Cliente.Cidade + '-' + Cliente.Estado, 40, ' '));

    if HabilitaCPFNoRodape then
      if (Cliente.Cgc <> '') and (Pos(Cliente.Cgc, msg) = 0) then
        fFechaCupomMsg.Add(Cliente.Cgc);

    msg := '';
    if (Pedido <> '') then
      msg := msg + ' Ped:' + Pedido;
    if (Vendedor <> '') then
      msg := msg + ' Vend:' + Vendedor;
    if (msg <> '') then
      fFechaCupomMsg.Add(msg);
    Result := fFechaCupomMsg;
  end;




function TFiscalBase.FormaPgtoMFD(str: string): string;
  var x:string;
    i:integer;
  begin
    result := str  ;

    x := FormaPgtoList.Values [str];
    if length(str)>2 then  // diferencia indice de texto da forma de pagamento.
      if x='' then           //  01 - indice      DIN - texto
        for I := 0 to FormaPgtoList.Count - 1 do      // so aplica busca parcial se for TEXTO
          if pos(FormaPgtoList.names[i],str)>0 then
            x := FormaPgtoList.ValueFromIndex[I];

    if x<>'' then
      result := x;
  end;

end.

