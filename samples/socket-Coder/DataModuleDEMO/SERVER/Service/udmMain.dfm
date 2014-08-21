object dmMain: TdmMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 291
  Width = 451
  object conMain: TADOConnection
    ConnectionString = 
      'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security In' +
      'fo=False;User ID=sa;Initial Catalog=master;Data Source=.'
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 24
    Top = 16
  end
  object dspMain: TDataSetProvider
    DataSet = qryMain
    Left = 144
    Top = 16
  end
  object qryMain: TADOQuery
    Connection = conMain
    Parameters = <>
    Left = 88
    Top = 16
  end
end
