object frmDoublyLink: TfrmDoublyLink
  Left = 0
  Top = 0
  Caption = 'frmDoublyLink'
  ClientHeight = 328
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 604
    Height = 281
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 128
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object btnSingleProduce: TButton
    Left = 232
    Top = 8
    Width = 97
    Height = 25
    Caption = 'btnSingleProduce'
    TabOrder = 3
    OnClick = btnSingleProduceClick
  end
  object btnSingleConsum: TButton
    Left = 360
    Top = 8
    Width = 121
    Height = 25
    Caption = 'btnSingleConsum'
    TabOrder = 4
    OnClick = btnSingleConsumClick
  end
end
