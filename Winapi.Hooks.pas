unit Winapi.Hooks;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes;

{$M+}

const
  MAX_KEY_NAME_LENGTH = 100;
  SHIFTED = $8000;

  (*
    * Low level hook flags
  *)
  LLKHF_EXTENDED = $01;
  LLKHF_INJECTED = $10;
  LLKHF_ALTDOWN = $20;
  LLKHF_UP = $80;

const
  VK_A = 65;
  VK_B = 66;
  VK_C = 67;
  VK_D = 68;
  VK_E = 69;
  VK_F = 70;
  VK_G = 71;
  VK_H = 72;
  VK_I = 73;
  VK_J = 74;
  VK_K = 75;
  VK_L = 76;
  VK_M = 77;
  VK_N = 78;
  VK_O = 79;
  VK_P = 80;
  VK_Q = 81;
  VK_R = 82;
  VK_S = 83;
  VK_T = 84;
  VK_U = 85;
  VK_V = 86;
  VK_W = 87;
  VK_X = 88;
  VK_Y = 89;
  VK_Z = 90;
  VK_0 = 48;
  VK_1 = 49;
  VK_2 = 50;
  VK_3 = 51;
  VK_4 = 52;
  VK_5 = 53;
  VK_6 = 54;
  VK_7 = 55;
  VK_8 = 56;
  VK_9 = 57;

type
  THook = class;
  THookMessage = TMessage;
  THookNotify = reference to procedure(Hook: THook; var HookMsg: THookMessage);

  TKeyState = (ksKeyDown = 0, ksKeyIsDown = 1, ksKeyUp = 2);
  pKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

  KBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    ScanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

  TKBDLLHookStruct = KBDLLHOOKSTRUCT;

  pLowLevelKeyStates = ^TLowLevelKeyStates;

  TLowLevelKeyStates = packed record
    ExtendKey: Boolean;
    InjectedKey: Boolean;
    AltDown: Boolean;
    CtrlDown: Boolean;
    ShiftDown: Boolean;
    KeyState: TKeyState;
    KeyboardState: TKeyboardState;
  end;

  pKeyNames = ^TKeyNames;

  TKeyNames = class
  public
    ScanCode: Integer;
    SpeciaKkey: Boolean;
    KeyExtName: string;
    procedure Clear;
  end;

  TKeyStates = packed record
    KeyState: TKeyState;
    KeyDown: Boolean;
    ShiftDown: Boolean;
    AltDown: Boolean;
    CtrlDown: Boolean;
    ExtendedKey: Boolean;
    MenuKey: Boolean;
    KeyRepeated: Boolean;
    RepeatCount: Integer;
    CharCount: Integer;
  end;

  TCustomHook = class abstract
  strict private
    FActive: Boolean;
    FHook: hHook;
    FHookProc: Pointer;
    FThreadID: Integer;

    FOnPreExecute: THookNotify;
    FOnPostExecute: THookNotify;
    procedure HookProc(var HookMsg: THookMessage);
    procedure SetActive(const Value: Boolean);
  private

  protected
    function GetHookID: Integer; virtual; abstract;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); virtual;
    procedure PostExecute(var HookMsg: THookMessage); virtual;

    property Active: Boolean read FActive write SetActive;
    property OnPreExecute: THookNotify read FOnPreExecute write FOnPreExecute;
    property OnPostExecute: THookNotify read FOnPostExecute write FOnPostExecute;

    property ThreadID: Integer read FThreadID write FThreadID;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  THook = class abstract(TCustomHook)
  published
    property Active;
    property OnPreExecute;
    property OnPostExecute;
    property ThreadID;
  end;

  TBaseKeyboardHook = class abstract(THook)
  strict private
    FKeyNames: TKeyNames;
  published
    property KeyName: TKeyNames read FKeyNames;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCallWndProcHook = class sealed(THook)
  private
    FCwpRetStruct: TCwpRetStruct;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property CwpRetStruct: TCwpRetStruct read FCwpRetStruct;
  end;

  TCallWndProcRetHook = class sealed(THook)
  private
    FCwpRetStruct: TCwpRetStruct;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property CwpRetStruct: TCwpRetStruct read FCwpRetStruct;
  end;

  TCBTHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TDebugHook = class sealed(THook)
  private
    FDebugHookInfo: TDebugHookInfo;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property DebugHookInfo: TDebugHookInfo read FDebugHookInfo;
  end;

  TGetMessageHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TJournalPlaybackHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TJournalRecordHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TKeyboardHook = class sealed(TBaseKeyboardHook)
  private
    FKeyState: TKeyStates;
  protected
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
    function GetHookID: Integer; override;
  public
    property KeyStates: TKeyStates read FKeyState;
  end;

  TMouseHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TMsgHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TShellHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TSysMsgHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TLowLevelKeyboardHook = class sealed(TBaseKeyboardHook)
  private
    FHookStruct: TKBDLLHookStruct;
    FLowLevelKeyStates: TLowLevelKeyStates;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property HookStruct: TKBDLLHookStruct read FHookStruct;
    property LowLevelKeyStates: TLowLevelKeyStates read FLowLevelKeyStates;
  end;

  TLowLevelMouseHook = class sealed(THook)
  strict private
  type
    pMSLLHOOKSTRUCT = ^MSLLHOOKSTRUCT;

    MSLLHOOKSTRUCT = packed record
      Pt: TPoint;
      MouseData: DWORD;
      flags: DWORD;
      time: DWORD;
      dwExtraInfo: ULONG_PTR;
    end;

    TMSLLHookStruct = MSLLHOOKSTRUCT;

  var
    FHookStruct: TMSLLHookStruct;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property HookStruct: TMSLLHookStruct read FHookStruct;
  end;

type
  THookContainer<T: THook, constructor> = class(TComponent)
  private
    FHook: T;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    class function Construct(AOwner: TComponent): T;
    property Hook: T read FHook;
  end;

  THookInstance<T: THook, constructor> = record
  public
    class function CreateHook(AOwner: TComponent): T; static;
  end;

implementation

uses
  System.SysUtils;

{ TLowLevelMouseHook }

function KeyIsDown(const nVirtKey: Integer): Boolean;
begin
  Result := (GetKeyState(nVirtKey) and SHIFTED) <> 0;
end;

function TLowLevelMouseHook.GetHookID: Integer;
begin
  Result := WH_MOUSE_LL;
end;

procedure TLowLevelMouseHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FHookStruct, SizeOf(FHookStruct));
end;

procedure TLowLevelMouseHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FHookStruct := pMSLLHOOKSTRUCT(HookMsg.WParam)^;
  inherited;
end;

{ TCustomHook }

constructor TCustomHook.Create;
begin
  inherited;
  FHookProc := MakeObjectInstance(HookProc);
  FHook := 0;
  FActive := False;
  FThreadID := GetCurrentThreadID;
end;

destructor TCustomHook.Destroy;
begin
  Active := False;
  FreeObjectInstance(FHookProc);
  inherited;
end;

procedure TCustomHook.HookProc(var HookMsg: THookMessage);
var
  Handled: Boolean;
begin
  Handled := False;
  PreExecute(HookMsg, Handled);
  if not Handled then
  begin
    HookMsg.Result := CallNextHookEx(FHook, HookMsg.Msg, HookMsg.WParam, HookMsg.LParam);
    PostExecute(HookMsg);
  end;
end;

procedure TCustomHook.PostExecute(var HookMsg: THookMessage);
begin
  if Assigned(FOnPostExecute) then
    FOnPostExecute(THook(Self), HookMsg)
end;

procedure TCustomHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  if Assigned(FOnPreExecute) then
    FOnPreExecute(THook(Self), HookMsg);

  Handled := HookMsg.Result <> 0;
end;

procedure TCustomHook.SetActive(const Value: Boolean);
var
  ID: Integer;
begin
  if FActive = Value then
    Exit;

  FActive := Value;

  case Active of
    True:
      begin
        ID := GetHookID;

        if ID in [WH_KEYBOARD_LL, WH_MOUSE_LL] then
          FThreadID := 0;

        FHook := SetWindowsHookEx(GetHookID, FHookProc, HInstance, FThreadID);
        if (FHook = 0) then
        begin
          FActive := False;
          raise Exception.Create(Classname + ' CREATION FAILED!');
        end;
      end;

    False:
      begin
        if (FHook <> 0) then
          UnhookWindowsHookEx(FHook);
        FHook := 0;
      end;
  end;
end;

{ TLowLevelKeyboardHook }

function TLowLevelKeyboardHook.GetHookID: Integer;
begin
  Result := WH_KEYBOARD_LL;
end;

procedure TLowLevelKeyboardHook.PostExecute(var HookMsg: THookMessage);
begin
  ZeroMemory(@FHookStruct, SizeOf(TKBDLLHookStruct));
  ZeroMemory(@FLowLevelKeyStates, SizeOf(TLowLevelKeyStates));
  KeyName.Clear;
  inherited;
end;

procedure TLowLevelKeyboardHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
  Function GetKeyName: String;
  var
    dwMsg: DWORD;
  begin
    dwMsg := 1;
    dwMsg := dwMsg + (FHookStruct.ScanCode shl 16);
    dwMsg := dwMsg + (FHookStruct.flags shl 24);
    SetLength(Result, 128);
    SetLength(Result, GetKeynameText(dwMsg, @Result[1], Length(Result)));
  end;

var
  KBS: TKeyboardState;
begin
  FHookStruct := pKBDLLHOOKSTRUCT(HookMsg.WParam)^;

  GetKeyboardState(KBS);
  Move(KBS, FLowLevelKeyStates.KeyboardState, SizeOf(KBS));

  with FLowLevelKeyStates do
  begin
    ExtendKey := (FHookStruct.flags and LLKHF_EXTENDED) <> 0;
    InjectedKey := (FHookStruct.flags and LLKHF_INJECTED) <> 0;
    AltDown := (FHookStruct.flags and LLKHF_ALTDOWN) <> 0;
    CtrlDown := FHookStruct.vkCode in [VK_LCONTROL, VK_RCONTROL];
    ShiftDown := FHookStruct.vkCode in [VK_LSHIFT, VK_RSHIFT];

    if (FHookStruct.flags and LLKHF_UP) <> 0 then
      KeyState := ksKeyUp
    else
      KeyState := ksKeyDown;
  end;

  KeyName.ScanCode := FHookStruct.ScanCode;
  KeyName.SpeciaKkey := (HookMsg.LParam and $1000000) <> 0;
  KeyName.KeyExtName := GetKeyName;

  inherited;
end;

{ TCallWndProcHook }

function TCallWndProcHook.GetHookID: Integer;
begin
  Result := WH_CALLWNDPROC;
end;

procedure TCallWndProcHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FCwpRetStruct, SizeOf(TCwpRetStruct));
end;

procedure TCallWndProcHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FCwpRetStruct := pCwpRetStruct(HookMsg.WParam)^;
  inherited;
end;

{ TCallWndProcRetHook }

function TCallWndProcRetHook.GetHookID: Integer;
begin
  Result := WH_CALLWNDPROCRET;
end;

procedure TCallWndProcRetHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FCwpRetStruct, SizeOf(TCwpRetStruct));
end;

procedure TCallWndProcRetHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FCwpRetStruct := pCwpRetStruct(HookMsg.LParam)^;
  inherited;
end;

{ TCBTHook }

function TCBTHook.GetHookID: Integer;
begin
  Result := WH_CBT;
end;

{ TDebugHook }

function TDebugHook.GetHookID: Integer;
begin
  Result := WH_DEBUG;
end;

procedure TDebugHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FDebugHookInfo, SizeOf(TDebugHookInfo));
end;

procedure TDebugHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FDebugHookInfo := pDebugHookInfo(HookMsg.LParam)^;
  inherited;
end;

{ TGetMessageHook }

function TGetMessageHook.GetHookID: Integer;
begin
  Result := WH_GETMESSAGE;
end;

{ TJournalPlaybackHook }

function TJournalPlaybackHook.GetHookID: Integer;
begin
  Result := WH_JOURNALPLAYBACK;
end;

{ TJournalRecordHook }

function TJournalRecordHook.GetHookID: Integer;
begin
  Result := WH_JOURNALRECORD;
end;

{ TKeyboardHook }

function TKeyboardHook.GetHookID: Integer;
begin
  Result := WH_KEYBOARD;
end;

procedure TKeyboardHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FKeyState, SizeOf(TKeyState));
  KeyName.Clear;
end;

procedure TKeyboardHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
  function GetKeyName: string;
  begin
    SetLength(Result, 128);
    SetLength(Result, GetKeynameText(HookMsg.WParam, @Result[1], Length(Result)));
  end;

var
  KBS: TKeyboardState;
begin
  FKeyState.KeyDown := (HookMsg.LParam and (1 shl 31)) = 0;
  FKeyState.KeyRepeated := (HookMsg.LParam and (1 shl 30)) = 0;
  FKeyState.AltDown := KeyIsDown(VK_MENU);
  FKeyState.MenuKey := (HookMsg.LParam and (1 shl 28)) = 0;
  FKeyState.ExtendedKey := (HookMsg.LParam and (1 shl 24)) = 0;
  FKeyState.CtrlDown := KeyIsDown(VK_CONTROL);
  FKeyState.ShiftDown := (GetKeyState(VK_SHIFT) and (1 shl 15)) = 0;
  FKeyState.KeyState := TKeyState(HookMsg.LParam shr 30);

  KeyName.ScanCode := HookMsg.Msg;

  if (FKeyState.KeyRepeated and FKeyState.KeyDown) then
    Inc(FKeyState.RepeatCount)
  else
    FKeyState.RepeatCount := 0;

  GetKeyboardState(KBS);

  KeyName.KeyExtName := GetKeyName;
  inherited;
end;

{ TMouseHook }

function TMouseHook.GetHookID: Integer;
begin
  Result := WH_MOUSE;
end;

{ TMsgHook }

function TMsgHook.GetHookID: Integer;
begin
  Result := WH_MSGFILTER;
end;

{ TShellHook }

function TShellHook.GetHookID: Integer;
begin
  Result := WH_SHELL;
end;

{ TSysMsgHook }

function TSysMsgHook.GetHookID: Integer;
begin
  Result := WH_SYSMSGFILTER;
end;

{ THookContainer<T> }

class function THookContainer<T>.Construct(AOwner: TComponent): T;
begin
  Result := THookContainer<T>.Create(AOwner).FHook;
end;

constructor THookContainer<T>.Create(AOwner: TComponent);
begin
  inherited;
  FHook := T.Create;
end;

destructor THookContainer<T>.Destroy;
begin
  FHook.Free;
  inherited;
end;

{ THookInstance<T> }

class function THookInstance<T>.CreateHook(AOwner: TComponent): T;
begin
  Result := THookContainer<T>.Construct(AOwner)
end;

{ TBaseKeyboardHook }

constructor TBaseKeyboardHook.Create;
begin
  inherited;
  FKeyNames := TKeyNames.Create;
end;

destructor TBaseKeyboardHook.Destroy;
begin
  FKeyNames.Free;
  inherited;
end;

{ TKeyNames }

procedure TKeyNames.Clear;
begin
  ScanCode := 0;
  SpeciaKkey := False;
  KeyExtName := string.empty;
end;

end.
