unit fireDacLoginDialogBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDac.Comp.Client;

type
  TFireDacLoginDlgBase = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    User_Name: TEdit;
    Password: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCanceled: boolean;
    FConnection: TFDCustomConnection;
    procedure SetCanceled(const Value: boolean);
    procedure SetConnection(const Value: TFDCustomConnection);
    { Private declarations }
  public
    { Public declarations }
   property  Canceled:boolean read FCanceled write SetCanceled;
   property Connection:TFDCustomConnection read FConnection write SetConnection;
  end;

var
  FireDacLoginDlgBase: TFireDacLoginDlgBase;

implementation

{$R *.dfm}

procedure TFireDacLoginDlgBase.Button1Click(Sender: TObject);
begin
   Canceled := false;
   ModalResult := mrOk;
   close;
end;


procedure TFireDacLoginDlgBase.Button2Click(Sender: TObject);
begin
  Canceled := true;
  ModalResult := mrCancel;
  close;
end;

procedure TFireDacLoginDlgBase.FormCreate(Sender: TObject);
begin
   Canceled := true;
   ModalResult := mrCancel;
end;

procedure TFireDacLoginDlgBase.FormShow(Sender: TObject);
begin
    if User_Name.Text<>'' then
       ActiveControl := Password;
end;

procedure TFireDacLoginDlgBase.SetCanceled(const Value: boolean);
begin
  FCanceled := Value;
end;

procedure TFireDacLoginDlgBase.SetConnection(const Value: TFDCustomConnection);
begin
  FConnection := Value;
end;

end.
