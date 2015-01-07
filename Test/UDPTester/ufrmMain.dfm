object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 306
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtHost: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 152
    Top = 8
    Width = 81
    Height = 21
    TabOrder = 1
    Text = '8983'
  end
  object btnSend: TButton
    Left = 270
    Top = 4
    Width = 75
    Height = 25
    Caption = 'btnSend'
    TabOrder = 2
    OnClick = btnSendClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 35
    Width = 337
    Height = 102
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  object mmoRecv: TMemo
    Left = 8
    Top = 200
    Width = 593
    Height = 89
    TabOrder = 4
  end
  object edtListen: TEdit
    Left = 8
    Top = 173
    Width = 121
    Height = 21
    TabOrder = 5
    Text = '8983'
  end
  object btnListen: TButton
    Left = 152
    Top = 169
    Width = 75
    Height = 25
    Caption = 'btnListen'
    TabOrder = 6
    OnClick = btnListenClick
  end
  object btnIOCPListen: TButton
    Left = 270
    Top = 169
    Width = 75
    Height = 25
    Caption = 'btnIOCPListen'
    TabOrder = 7
    OnClick = btnIOCPListenClick
  end
  object Button1: TButton
    Left = 456
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 8
    OnClick = Button1Click
  end
  object tmrRecv: TTimer
    Enabled = False
    Left = 568
    Top = 88
  end
end
