unit VCLFullsizeForm;

interface

uses
  FMX.Forms { must be included before Vcl.Forms so that 'TForm' below refers to a VCL form, not FMX},
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FMXForm, FMX3DForm, Parnassus.FMXContainer, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    FireMonkeyContainer: TFireMonkeyContainer;
    procedure FireMonkeyContainerCreateFMXForm(var Form: TCommonCustomForm);
    procedure FireMonkeyContainerDestroyFMXForm(var Form: TCommonCustomForm;
      var Action: TCloseHostedFMXFormAction);
  private
    { Private declarations }
    procedure OpenAnotherFormClick(Sender: TObject);
    procedure OpenAnother3DFormClick(Sender: TObject);
  public
    { Public declarations }
    Is3DForm: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
    FMX.Types;

procedure TForm1.OpenAnotherFormClick(Sender: TObject);
begin
  TForm1.Create(Application).Show;
end;

procedure TForm1.OpenAnother3DFormClick(Sender: TObject);
begin
  with TForm1.Create(Application) do
  begin
    Is3DForm := True;
    Show;
  end;
end;


procedure TForm1.FireMonkeyContainerCreateFMXForm(var Form: TCommonCustomForm);
var
    FmForm: TFireMonkeyForm;
begin
  if not Assigned(Form) then
  begin
    if not Is3DForm then
    begin
      FmForm := TFireMonkeyForm.Create(Self);
      FmForm.GroupBox1.Align := TAlignLayout.alClient;
      FmForm.Button1.OnClick := OpenAnotherFormClick;
      FmForm.Button2.OnClick := OpenAnother3DFormClick;
      Form := FmForm;
    end
    else
      Form := TFormExample3D.Create(Self);
  end;
end;

procedure TForm1.FireMonkeyContainerDestroyFMXForm(var Form: TCommonCustomForm;
  var Action: TCloseHostedFMXFormAction);
begin
  Action := fcaNone;
end;

end.
