unit Parnassus.FmxVclForm;

{
    The goal of this unit is that, if you have a FMX form TMyFmxForm
    you should be able to use this form in a VCL application as normal as possible by creating
    the form in the following way:

    MyWrappedForm := TFmxVclForm.CreateNew(Application, TMyFmxForm);

}

interface

uses
    Parnassus.FmxContainer,
    FMX.Forms,
    Winapi.Messages,
    Winapi.Windows,
    System.Classes,
    Vcl.StdCtrls,
    Vcl.Forms;

type
    TFmxFormClass = class of TCommonCustomForm;

    TFmxVclForm = class(Vcl.Forms.TCustomForm)
    private
        FFormClass: TFmxFormClass;
        FForm: TCommonCustomForm;
    protected
        FFireMonkeyContainer: TFireMonkeyContainer;
        procedure FireMonkeyContainerCreateFMXForm(var Form: TCommonCustomForm);
        procedure FireMonkeyContainerDestroyFMXForm(var Form: TCommonCustomForm;
          var Action: TCloseHostedFMXFormAction);
        function PropGetFireMonkeyForm: TCommonCustomForm;
        procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;

        function ShowModal: Integer; override;
        procedure WndProc(var msg : TMessage); override;

        class function FmxToVclBorderStyle(AStyle: TFMXFormBorderStyle): TFormBorderStyle;
        class function FmxToVclFormPosition(APos: TFormPosition): TPosition;
        constructor Create(AOwner: TComponent); override;
    public
        property FireMonkeyForm: TCommonCustomForm read PropGetFireMonkeyForm;
        constructor CreateNew(AOwner: TComponent; FormClass: TFmxFormClass); reintroduce; virtual;
    public
        class function FmxToVclForm(AFmxForm: TCommonCustomForm): TFmxVclForm;
    end;

implementation

uses
    SysUtils,
    Vcl.Controls;

constructor TFmxVclForm.Create(AOwner: TComponent);
begin
    raise Exception.Create('TFmxVclForm does not support Create(). Please use CreateNew() instead!');
end;

constructor TFmxVclForm.CreateNew(AOwner: TComponent; FormClass: TFmxFormClass);
begin
    // From the docu:
    // Use CreateNew instead of Create to create a form without using the associated .DFM file to initialize it.
    inherited CreateNew(AOwner);
    FFormClass := FormClass;
    //programmatically create FireMonkeyContainer
    FFireMonkeyContainer := TFireMonkeyContainer.Create(Self);
    FFireMonkeyContainer.Align := alClient;
    FFireMonkeyContainer.OnCreateFMXForm := FireMonkeyContainerCreateFMXForm;
    FFireMonkeyContainer.OnDestroyFMXForm := FireMonkeyContainerDestroyFMXForm;
    FFireMonkeyContainer.Parent := Self;
    //create Form - this is necessary this early, because inherited classes may need this before
    //FireMonkeyContainerCreateFMXForm is called
    FForm := FFormClass.Create(Self);
    Self.Height := FForm.Height;
    Self.Width := FForm.Width;
    Self.BorderIcons := FForm.BorderIcons;
    Self.BorderStyle := FmxToVclBorderStyle(FForm.BorderStyle);
    Self.Caption := FForm.Caption;
    Self.Cursor := FForm.Cursor;
    Self.Left := FForm.Left;
    Self.Position := FmxToVclFormPosition(FForm.Position);
    Self.WindowState := FForm.WindowState;
end;

procedure TFmxVclForm.FireMonkeyContainerCreateFMXForm(var Form: TCommonCustomForm);
begin
    if not Assigned(Form) then
      Form := FForm;
end;

procedure TFmxVclForm.FireMonkeyContainerDestroyFMXForm(var Form: TCommonCustomForm;
  var Action: TCloseHostedFMXFormAction);
begin
    Action := fcaFree;
end;

class function TFmxVclForm.FmxToVclBorderStyle(AStyle: TFMXFormBorderStyle): TFormBorderStyle;
var
    Tmp: Integer;
begin
    //FMX: (bsNone, bsSingle, bsSizeable,           bsToolWindow, bsSizeToolWin);
    //VCL: (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);
    Tmp := Integer(AStyle);
    if Tmp >= Integer(bsToolWindow) then
      Inc(Tmp);
    Result := TFormBorderStyle(Tmp);
end;

class function TFmxVclForm.FmxToVclForm(AFmxForm: TCommonCustomForm): TFmxVclForm;
begin
    Assert(Assigned(AFmxForm));
    //---
    //Owner should always be the FmxVclForm
    if AFmxForm.Owner is TFmxVclForm then
      Result := AFmxForm.Owner as TFmxVclForm
    else
      Result := nil;
end;

class function TFmxVclForm.FmxToVclFormPosition(APos: TFormPosition): TPosition;
begin
    //FMX: TFormPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);
    //VCL: TPosition =     (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);
    Result := TPosition(APos);
end;

function TFmxVclForm.PropGetFireMonkeyForm: TCommonCustomForm;
begin
    Result := FForm;
    //this might not be available yet: Result := FFireMonkeyContainer.FireMonkeyForm;
end;


procedure TFmxVclForm.WMActivate(var Msg: TWMActivate);
begin
  if not (GetWindowLong(Handle, GWL_STYLE) and WS_CHILD = WS_CHILD) and (FormStyle <> fsMDIForm) then
    if Msg.Active <> WA_INACTIVE then
      FForm.Active := True;
    //better??
    //FForm.Active := (Msg.Active <> WA_INACTIVE);
end;


//This function and TFmxVclForm.WndProc play together to allow ShowModal of the VCL form while listening to
//ModalResult values of the FMX form
function TFmxVclForm.ShowModal: Integer;
begin
    //without this the form will close immediately once FForm ModalResult had been modified before
    if Assigned(FForm) then
      FForm.ModalResult := 0;
    Result := inherited ShowModal;
end;


//Overwrite message handling to grab changes to ModalResult
procedure TFmxVclForm.WndProc(var msg: TMessage);
begin
  inherited;
  // get ModalResult of wrapped form. Otherwise TButton.ModalResult etc. wont work
  // VCL's ShowModal will check ModalResult in a loop where only Application.HandleMessage is called
  if (ModalResult = 0) and (Assigned(FForm)) and (FForm.ModalResult <> 0) then
    ModalResult := FForm.ModalResult;
end;

end.
