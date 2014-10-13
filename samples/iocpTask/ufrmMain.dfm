object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 378
  ClientWidth = 713
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
    Left = 89
    Top = 8
    Width = 113
    Height = 25
    Caption = 'SpeedTester'
    TabOrder = 2
    OnClick = SpeedTesterClick
  end
  object btnState: TButton
    Left = 424
    Top = 8
    Width = 99
    Height = 25
    Caption = 'get state info'
    TabOrder = 3
    OnClick = btnStateClick
  end
  object btnSignal: TButton
    Left = 544
    Top = 100
    Width = 89
    Height = 25
    Caption = 'signal a task'
    TabOrder = 4
    OnClick = btnSignalClick
  end
  object btnRegister: TButton
    Left = 544
    Top = 37
    Width = 75
    Height = 25
    Caption = 'btnRegister'
    TabOrder = 5
    OnClick = btnRegisterClick
  end
  object btnUnRegister: TButton
    Left = 544
    Top = 69
    Width = 75
    Height = 25
    Caption = 'btnUnRegister'
    TabOrder = 6
    OnClick = btnUnRegisterClick
  end
end
