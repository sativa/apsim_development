object ObjectInspectorForm: TObjectInspectorForm
  Left = 192
  Top = 117
  Width = 225
  Height = 488
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Object Inspector'
  Color = clBtnFace
  UseDockManager = True
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ObjectInspector: TSEGObjectInspector
    Left = 0
    Top = 0
    Width = 217
    Height = 454
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
  end
end
