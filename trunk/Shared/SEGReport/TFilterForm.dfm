inherited FilterForm: TFilterForm
  Caption = 'Filter form'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited PropertiesSheet: TTabSheet
      object Label3: TLabel
        Left = 0
        Top = 32
        Width = 25
        Height = 13
        Caption = 'Filter:'
      end
      object FilterEdit: TEdit
        Left = 8
        Top = 48
        Width = 201
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = FilterEditExit
      end
    end
  end
end
