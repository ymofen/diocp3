object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 327
  ClientWidth = 786
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
    Width = 320
    Height = 209
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btn1: TButton
    Left = 104
    Top = 8
    Width = 113
    Height = 25
    Caption = 'btn1'
    TabOrder = 2
    OnClick = btn1Click
  end
  object btnSimpMsgPack: TButton
    Left = 223
    Top = 8
    Width = 113
    Height = 25
    Caption = 'btnSimpMsgPack'
    TabOrder = 3
    OnClick = btnSimpMsgPackClick
  end
  object btnGetFirst: TButton
    Left = 360
    Top = 8
    Width = 121
    Height = 25
    Caption = 'btnGetFirst'
    TabOrder = 4
    OnClick = btnGetFirstClick
  end
  object Memo2: TMemo
    Left = 464
    Top = 56
    Width = 265
    Height = 209
    Lines.Strings = (
      'Memo2')
    TabOrder = 5
  end
  object btn2: TButton
    Left = 272
    Top = 296
    Width = 75
    Height = 25
    Caption = 'btn2'
    TabOrder = 6
    OnClick = btn2Click
  end
end
