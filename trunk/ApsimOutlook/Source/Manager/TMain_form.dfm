�
 TMAIN_FORM 0�
  TPF0
TMain_form	Main_formLeft0Top+Width�HeightwCaptionSimulation database managerColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameSystem
Font.Style MenuMainMenuOldCreateOrder	OnCreate
FormCreateOnShowFormShow
PixelsPerInch`
TextHeight TLabelPrompt_labelLeft Top Width�HeightAlignalTop	AlignmenttaCenterCaption$List of all simulations in database:Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  
TStatusBar	StatusBarLeft Top)Width�HeightPanelsWidth, Width�   SimplePanel  TDBGridSimulation_name_gridLeft TopWidth�HeightAlignalClient
DataSourceDataSource1Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderTitleFont.CharsetANSI_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameArialTitleFont.StylefsBold ColumnsExpanded	FieldNameNameReadOnly	Width�Visible	    	TMainMenuMainMenuLeft�TopH 	TMenuItemFileMenuCaption&File 	TMenuItemOpen_database_menuCaption&Open database...HintOpen a simulation databaseOnClick
Open_database  	TMenuItemN1Caption-  	TMenuItemImport_simulation_menuCaption&Import simulations...Hint)Import simulations from one or more filesOnClickImport_simulation  	TMenuItem%Import_simulation_using_filespec_menuCaption!Import simulation using &filespecHint+Import simulations from one or more foldersOnClick Import_simulation_using_filespec  	TMenuItemBatchimportusingfilespec1Caption&Batch import using filespecHint*Import simulations into multiple databasesOnClickBatchimportusingfilespec1Click  	TMenuItemN2Caption-  	TMenuItem	Exit_menuCaptionE&xitHintExitShortCutX�  OnClickExit   	TMenuItemOptions1Caption&Options 	TMenuItemCheckforduplicatesimulationsCaption)&Check database for duplicate simulationsChecked	OnClick!CheckforduplicatesimulationsClick    TOpenDialogDatabase_open_dialog
DefaultExt.mdbFilter0Database Files (*.mdb)|*.mdb|All Files (*.*)|*.*Left�Top`  TSaveDialog
SaveDialogFilterAll Files (*.*)|*.*Left�Topx  TDataSourceDataSource1DataSetIndex_tableLeft�Top�   TOpenDialogSimulation_open_dialog
DefaultExt.outFilter2Simulation files (*.out)|*.out|All files (*.*)|*.*OptionsofAllowMultiSelectofPathMustExistofFileMustExist Left�Top�   	TADOTableIndex_table
ConnectionSimulation_database
CursorTypectStatic	TableName[Index]Left�Top�   TSimulation_databaseSimulation_databaseLoginPromptProviderMicrosoft.Jet.OLEDB.4.0
CheckIfExists	Left�Top   