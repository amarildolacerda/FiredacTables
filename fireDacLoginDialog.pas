unit FireDacLoginDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDac.Comp.Client;

type
  TFireDacLoginDlg = class(TForm)
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
    { Private declarations }
  public
    { Public declarations }
    Canceled:boolean;
    Connection:TFDCustomConnection;
  end;

var
  FireDacLoginDlg: TFireDacLoginDlg;

implementation

{$R *.dfm}

procedure TFireDacLoginDlg.Button1Click(Sender: TObject);
begin
   Canceled := false;
   close;
end;

procedure TFireDacLoginDlg.Button2Click(Sender: TObject);
begin
  Canceled := true;
  close;
end;

procedure TFireDacLoginDlg.FormCreate(Sender: TObject);
begin
   Canceled := true;
end;

procedure TFireDacLoginDlg.FormShow(Sender: TObject);
begin
    if User_Name.Text<>'' then
       ActiveControl := Password;
end;

end.
