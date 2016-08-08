unit Data.FireTables.GuiXDialogs;

interface

uses
  System.SysUtils, System.Classes, FireDAC.UI.Intf, FireDAC.VCLUI.Async,
  FireDAC.Stan.Intf, FireDAC.Comp.UI;

type
  TFireTablesGuiXDialogs = class(TDataModule)
    FDGUIxAsyncExecuteDialog1: TFDGUIxAsyncExecuteDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FireTablesGuiXDialogs: TFireTablesGuiXDialogs;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
