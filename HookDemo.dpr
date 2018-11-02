program HookDemo;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {FormMain},
  Winapi.Hooks in 'Winapi.Hooks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
