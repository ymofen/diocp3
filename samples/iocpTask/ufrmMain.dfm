object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 362
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnPostTask: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnPostTask'
    TabOrder = 0
    OnClick = btnPostTaskClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 521
    Height = 315
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object SpeedTester: TButton
    Left = 120
    Top = 8
    Width = 113
    Height = 25
    Caption = 'SpeedTester'
    TabOrder = 2
    OnClick = SpeedTesterClick
  end
end
