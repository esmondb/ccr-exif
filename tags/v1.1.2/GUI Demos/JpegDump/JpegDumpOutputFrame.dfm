object OutputFrame: TOutputFrame
  AlignWithMargins = True
  Left = 0
  Top = 0
  Width = 389
  Height = 326
  TabOrder = 0
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 230
    Height = 326
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WantReturns = False
  end
  object grpExifThumbnail: TGroupBox
    AlignWithMargins = True
    Left = 236
    Top = 0
    Width = 153
    Height = 326
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alRight
    Caption = ' Exif thumbnail '
    TabOrder = 1
    Visible = False
    object imgExifThumbnail: TImage
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 143
      Height = 303
      Align = alClient
      ExplicitLeft = 176
      ExplicitTop = 0
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
  end
end
