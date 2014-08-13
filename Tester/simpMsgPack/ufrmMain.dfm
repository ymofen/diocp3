object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 309
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 56
    Width = 393
    Height = 201
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btn1: TButton
    Left = 432
    Top = 64
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 2
    OnClick = btn1Click
  end
  object btnSimpMsgPack: TButton
    Left = 432
    Top = 112
    Width = 75
    Height = 25
    Caption = 'btnSimpMsgPack'
    TabOrder = 3
    OnClick = btnSimpMsgPackClick
  end
end
