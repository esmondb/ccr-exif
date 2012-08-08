object OutputFrame: TOutputFrame
  AlignWithMargins = True
  Left = 0
  Top = 0
  Width = 501
  Height = 214
  ParentBackground = False
  TabOrder = 0
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 501
    Height = 1
    Align = alTop
    Pen.Color = clBtnShadow
    ExplicitLeft = 220
    ExplicitTop = 76
    ExplicitWidth = 65
  end
  object Shape2: TShape
    Left = 0
    Top = 1
    Width = 1
    Height = 196
    Align = alLeft
    Pen.Color = clBtnShadow
    ExplicitHeight = 501
  end
  object Shape3: TShape
    Left = 500
    Top = 1
    Width = 1
    Height = 196
    Align = alRight
    Pen.Color = clBtnShadow
    ExplicitLeft = 4
    ExplicitTop = 5
    ExplicitHeight = 194
  end
  object redRawXML: TRichEdit
    Left = 1
    Top = 1
    Width = 499
    Height = 196
    Align = alClient
    BevelEdges = [beLeft, beTop, beBottom]
    BevelInner = bvNone
    BevelOuter = bvSpace
    BevelKind = bkTile
    BorderStyle = bsNone
    ParentFont = False
    PopupMenu = mnuEdit
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    Visible = False
    WantReturns = False
  end
  object TabSet: TTabSet
    Left = 0
    Top = 197
    Width = 501
    Height = 17
    Align = alBottom
    DitherBackground = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    SelectedColor = clWindow
    SoftTop = True
    Style = tsSoftTabs
    Tabs.Strings = (
      '   &Properties'
      '   &Raw XML')
    TabIndex = 0
    UnselectedColor = clBtnFace
    OnChange = TabSetChange
  end
  object panProps: TPanel
    Left = 1
    Top = 1
    Width = 499
    Height = 196
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object Splitter: TSplitter
      Left = 258
      Top = 1
      Height = 194
      AutoSnap = False
      Beveled = True
      MinSize = 100
      ResizeStyle = rsUpdate
      ExplicitLeft = 257
      ExplicitTop = 0
      ExplicitHeight = 196
    end
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 257
      Height = 194
      Align = alLeft
      BorderStyle = bsNone
      HotTrack = True
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnChange = TreeViewChange
      OnClick = TreeViewClick
    end
    object memValue: TMemo
      Left = 261
      Top = 1
      Width = 237
      Height = 194
      Align = alClient
      BorderStyle = bsNone
      PopupMenu = mnuEdit
      ReadOnly = True
      TabOrder = 1
    end
  end
  object mnuEdit: TPopupMenu
    OnPopup = mnuEditPopup
    Left = 12
    Top = 16
    object itmCopySelText: TMenuItem
      Caption = 'Copy Selected Text'
      OnClick = itmCopySelTextClick
    end
    object itmSelectAll: TMenuItem
      Caption = 'Select All'
      OnClick = itmSelectAllClick
    end
  end
end
