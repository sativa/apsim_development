inherited FilterForm: TFilterForm
  Caption = 'Filter form'
  OldCreateOrder = True
  PixelsPerInch = 120
  TextHeight = 16
  inherited AdvPanelGroup1: TAdvPanelGroup
    FullHeight = 18
    inherited AdvancedPanel: TAdvPanel
      FullHeight = 293
    end
    inherited PropertyPanel: TAdvPanel
      FullHeight = 297
      object FilterLabel: TLabel
        Left = 16
        Top = 32
        Width = 39
        Height = 16
        Caption = 'Filter:'
      end
      object FilterEdit: TEdit
        Left = 16
        Top = 56
        Width = 233
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        BevelKind = bkSoft
        BorderStyle = bsNone
        TabOrder = 0
        OnExit = FilterEditExit
      end
    end
  end
end
