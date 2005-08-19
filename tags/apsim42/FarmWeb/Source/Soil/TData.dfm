object Data: TData
  OldCreateOrder = False
  Left = 520
  Top = 17
  Height = 232
  Width = 414
  object connection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=c:\mydata\Developme' +
      'nt\APSoil\apsru.mdb;Persist Security Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 40
    Top = 8
  end
  object region: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'Region'
    Left = 40
    Top = 56
  end
  object site: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'Site'
    Left = 88
    Top = 56
  end
  object soil: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'Soil'
    Left = 136
    Top = 56
  end
  object soilData: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'SoilData'
    Left = 184
    Top = 56
  end
  object crop: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'Crop'
    Left = 256
    Top = 104
  end
  object cropData: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'CropData'
    Left = 336
    Top = 56
  end
  object soilLayeredData: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'SoilLayeredData'
    Left = 256
    Top = 56
  end
  object soilType: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'SoilType'
    Left = 40
    Top = 104
  end
  object predictedLLConstants: TADOTable
    Connection = connection
    CursorLocation = clUseServer
    TableName = 'PredictedLLConstants'
    Left = 136
    Top = 104
  end
end
