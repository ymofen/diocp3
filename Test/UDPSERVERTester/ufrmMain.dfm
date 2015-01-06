object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 369
  ClientWidth = 767
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtListen: TEdit
    Left = 8
    Top = 21
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '8983'
  end
  object btnListen: TButton
    Left = 152
    Top = 17
    Width = 75
    Height = 25
    Caption = 'btnListen'
    TabOrder = 1
    OnClick = btnListenClick
  end
  object mmoRecv: TMemo
    Left = 8
    Top = 48
    Width = 593
    Height = 89
    TabOrder = 2
  end
end
