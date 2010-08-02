unit CCR.SizeGripCtrl;

interface

uses
  Windows, SysUtils, Classes, Controls;

type
  TSizeGrip = class(TWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Visible;
  end;

implementation

constructor TSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csOpaque, csFixedWidth, csFixedHeight];
  Anchors := [akRight, akBottom];
  Cursor  := crSizeNWSE;
end;

procedure TSizeGrip.CreateParams(var Params: TCreateParams);
var
  R: TRect;
begin
  inherited;
  CreateSubClass(Params, 'SCROLLBAR');
  Params.Style := Params.Style or WS_CLIPSIBLINGS or SBS_SIZEGRIP or
    SBS_SIZEBOXBOTTOMRIGHTALIGN;
  if not Windows.GetClientRect(Params.WndParent, R) then RaiseLastOSError;
  Params.X := R.Left;
  Params.Y := R.Top;
  Params.Width := R.Right - R.Left;
  Params.Height := R.Bottom - R.Top;
end; //the default VCL code will update our Left, Top, Width and Height props

end.
