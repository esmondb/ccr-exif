object dtmController: TdtmController
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 301
  object ActionList: TActionList
    Left = 24
    Top = 54
    object actOpen: TAction
      Category = 'File'
      Text = '&Open...'
      ShortCut = 4175
      OnExecute = actOpenExecute
    end
    object actClose: TWindowClose
      Category = 'File'
      CustomText = 'Close'
    end
    object actAppQuit: TFileExit
      Category = 'Application'
    end
    object actAppHide: TFileHideApp
      Category = 'Application'
    end
    object actAppHideOthers: TFileHideAppOthers
      Category = 'Application'
    end
    object actNextTab: TAction
      Category = 'Tab'
      ShortCut = 16393
      SecondaryShortCuts.Strings = (
        'Shift+Cmd+]')
      OnExecute = actNextTabExecute
    end
    object actPreviousTab: TAction
      Category = 'Tab'
      ShortCut = 24585
      SecondaryShortCuts.Strings = (
        'Shift+Cmd+[')
      OnExecute = actPreviousTabExecute
    end
    object actToggleFullScreen: TAction
      Category = 'Window'
      Text = 'Enter Full Screen'
      ShortCut = 20550
    end
    object actMinimizeWindow: TAction
      Category = 'Window'
      Text = 'Minimize'
      ShortCut = 4173
    end
    object actZoomWindow: TAction
      Category = 'Window'
      Text = 'Zoom'
    end
    object actAppShowAll: TAction
      Category = 'Application'
      Text = 'Show All'
    end
    object actBringAllToFront: TAction
      Category = 'Window'
      Text = 'Bring All to Front'
    end
    object actAppAbout: TAction
      Category = 'Application'
      Text = 'About IPTCEditor'
    end
    object actSave: TAction
      Category = 'File'
      Text = 'Save'
      OnExecute = actSaveExecute
      OnUpdate = actReloadOrSaveUpdate
    end
    object actReload: TAction
      Category = 'File'
      Text = 'Reload'
      OnExecute = actReloadExecute
      OnUpdate = actReloadOrSaveUpdate
    end
    object actClearAll: TAction
      Category = 'Edit'
      Text = 'Clear All'
      OnExecute = actClearAllExecute
    end
    object actAddNewItem: TAction
      Category = 'Grid'
      Text = 'Add New Item'
      OnExecute = actAddNewItemExecute
      OnUpdate = actAddNewItemUpdate
    end
    object actRemoveSelectedItem: TAction
      Category = 'Grid'
      Text = 'Remove Selected Item'
      OnExecute = actRemoveSelectedItemExecute
      OnUpdate = actRemoveSelectedItemUpdate
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'jpg'
    Filter = 
      'All supported image types|*.jpg;*.jpeg;*.tif;*.tiff|JPEG images ' +
      '(*.jpg,*.jpeg)|*.jpg;*.jpeg|TIFF images (*.tif,*.tiff)|*.tif;*.t' +
      'iff'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 87
    Top = 54
  end
  object MacMenu: TMainMenu
    Left = 151
    Top = 54
    object itmApp: TMenuItem
      Height = 50.000000000000000000
      Text = 'IPTCEditor'
      Width = 50.000000000000000000
      object MenuItem5: TMenuItem
        Action = actAppAbout
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmDiv: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object itmHideMe: TMenuItem
        Action = actAppHide
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmHideOtherApps: TMenuItem
        Action = actAppHideOthers
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmShowAll: TMenuItem
        Action = actAppShowAll
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem6: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object itmAppQuit: TMenuItem
        Action = actAppQuit
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
    object itmFile: TMenuItem
      Height = 50.000000000000000000
      Text = '&File'
      Width = 50.000000000000000000
      object itmFileOpen: TMenuItem
        Action = actOpen
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmFileClose: TMenuItem
        Action = actClose
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmFileReload: TMenuItem
        Action = actReload
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
    object MenuItem3: TMenuItem
      Height = 50.000000000000000000
      Text = 'Edit'
      Width = 50.000000000000000000
      object itmClearAll: TMenuItem
        Action = actClearAll
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem4: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object MenuItem7: TMenuItem
        Action = actAddNewItem
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem8: TMenuItem
        Action = actRemoveSelectedItem
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
    object itmWindow: TMenuItem
      Height = 50.000000000000000000
      Text = 'Window'
      Width = 50.000000000000000000
      object itmMinimize: TMenuItem
        Action = actMinimizeWindow
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem1: TMenuItem
        Action = actZoomWindow
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem2: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object itmBringAllToFront: TMenuItem
        Action = actBringAllToFront
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
  end
  object mnuRepeatingValue: TPopupMenu
    Left = 231
    Top = 54
    object itmAddItem: TMenuItem
      Action = actAddNewItem
      Height = 50.000000000000000000
      Width = 50.000000000000000000
    end
    object itmRemoveItem: TMenuItem
      Action = actRemoveSelectedItem
      Height = 50.000000000000000000
      Locked = True
      Width = 50.000000000000000000
    end
  end
end
