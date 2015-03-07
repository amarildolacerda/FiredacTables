object FireDacLoginDlg: TFireDacLoginDlg
  Left = 0
  Top = 0
  ActiveControl = User_Name
  Caption = 'Login Banco Dados'
  ClientHeight = 182
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 13
    Top = 59
    Width = 46
    Height = 13
    Caption = 'Usu'#225'rio:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 21
    Top = 95
    Width = 38
    Height = 13
    Caption = 'Senha:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 269
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Entre com usuario e senha de acesso'
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
  end
  object User_Name: TEdit
    Left = 74
    Top = 56
    Width = 169
    Height = 21
    TabOrder = 1
  end
  object Password: TEdit
    Left = 74
    Top = 92
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 74
    Top = 121
    Width = 75
    Height = 42
    Caption = 'Entrar'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 121
    Width = 75
    Height = 42
    Caption = 'Cancelar'
    TabOrder = 4
    OnClick = Button2Click
  end
end
