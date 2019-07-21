unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Threading, Vcl.StdCtrls, Vcl.ExtCtrls,

  Winapi.Hooks;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FHook: THook;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.Button1Click(Sender: TObject);
const
  Captions: array [Boolean] of string = ('Decativate', 'Active');
begin
  FHook.Active := not FHook.Active;
  Button1.Caption := Captions[not FHook.Active];
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FHook := THookInstance<TLowLevelKeyboardHook>.CreateHook(Self);
  FHook.OnPreExecute := procedure(Hook: THook; var HookMsg: THookMessage)
    var
      LLKeyBoardHook: TLowLevelKeyboardHook;
      ScanCode: integer;
    begin
      LLKeyBoardHook := TLowLevelKeyboardHook(Hook);

      if LLKeyBoardHook.LowLevelKeyStates.KeyState <> ksKeyDown then
        exit;

      ScanCode := LLKeyBoardHook.KeyName.ScanCode;

      if not(ScanCode in [VK_NUMPAD0 .. VK_NUMPAD9, VK_0 .. VK_9]) then
      begin
        Caption := 'Got ya! Key [' + LLKeyBoardHook.KeyName.KeyExtName + '] blocked.';
        HookMsg.Result := 1;
      end
      else
        Caption := '';
    end;
end;

end.
