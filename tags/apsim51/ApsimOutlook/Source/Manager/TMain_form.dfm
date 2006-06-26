object Main_form: TMain_form
  Left = 304
  Top = 299
  Width = 670
  Height = 375
  Caption = 'Simulation database manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Prompt_label: TLabel
    Left = 0
    Top = 0
    Width = 662
    Height = 15
    Align = alTop
    Alignment = taCenter
    Caption = 'List of all simulations in database:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 296
    Width = 662
    Height = 24
    Panels = <
      item
        Width = 300
      end
      item
        Width = 150
      end>
    SimplePanel = False
  end
  object Simulation_name_grid: TDBGrid
    Left = 0
    Top = 15
    Width = 662
    Height = 281
    Align = alClient
    DataSource = DataSource1
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Arial'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        ReadOnly = True
        Width = 914
        Visible = True
      end>
  end
  object MainMenu: TMainMenu
    Left = 400
    Top = 72
    object FileMenu: TMenuItem
      Caption = '&File'
      object Open_database_menu: TMenuItem
        Caption = '&Open database...'
        Hint = 'Open a simulation database'
        OnClick = Open_database
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Import_simulation_menu: TMenuItem
        Caption = '&Import simulations...'
        Hint = 'Import simulations from one or more files'
        OnClick = Import_simulation
      end
      object Import_simulation_using_filespec_menu: TMenuItem
        Caption = 'Import simulation using &filespec'
        Hint = 'Import simulations from one or more folders'
        OnClick = Import_simulation_using_filespec
      end
      object Batchimportusingfilespec1: TMenuItem
        Caption = '&Batch import using filespec'
        Hint = 'Import simulations into multiple databases'
        OnClick = Batchimportusingfilespec1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit_menu: TMenuItem
        Caption = 'E&xit'
        Hint = 'Exit'
        ShortCut = 32856
        OnClick = Exit
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Checkforduplicatesimulations: TMenuItem
        Caption = '&Check database for duplicate simulations'
        Checked = True
        OnClick = CheckforduplicatesimulationsClick
      end
    end
  end
  object Database_open_dialog: TOpenDialog
    DefaultExt = '.mdb'
    Filter = 'Database Files (*.mdb)|*.mdb|All Files (*.*)|*.*'
    Left = 400
    Top = 96
  end
  object SaveDialog: TSaveDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 400
    Top = 120
  end
  object DataSource1: TDataSource
    DataSet = Index_table
    Left = 400
    Top = 200
  end
  object Simulation_open_dialog: TOpenDialog
    DefaultExt = '.out'
    Filter = 'Simulation files (*.out)|*.out|All files (*.*)|*.*'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist]
    Left = 400
    Top = 232
  end
  object Index_table: TADOTable
    Connection = Simulation_database
    CursorType = ctStatic
    TableName = '[Index]'
    Left = 400
    Top = 168
  end
  object Simulation_database: TSimulation_database
    LoginPrompt = False
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    CheckIfExists = True
    Left = 400
    Top = 264
  end
end
