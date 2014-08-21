unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, iocpTask, StdCtrls;

type
  TfrmMain = class(TForm)
    btnPostTask: TButton;
    Memo1: TMemo;
    SpeedTester: TButton;
    Button1: TButton;
    procedure btnPostTaskClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedTesterClick(Sender: TObject);
  private
    FLogTask: TIocpTaskMananger;
    { Private declarations }
    procedure onLogMsg(pvStrData:string);

    procedure logMessage(pvMsg:string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
    procedure OnTaskWork();overload;
    procedure OnTaskWork(pvStrData:string);overload;

    procedure DoJobProc(pvTaskRequest:TIocpTaskRequest);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{$IFDEF MSWINDOWS}
type
  TGetTickCount64 = function: Int64;
{$ENDIF MSWINDOWS}

var
{$IFDEF NEXTGEN}
  _Watch: TStopWatch;
{$ELSE}
  GetTickCount64: TGetTickCount64;
  _PerfFreq: Int64;
{$ENDIF}

procedure TaskProcGlobal(pvTaskRequest: TIocpTaskRequest);
begin
  pvTaskRequest.Remark := '≤‚ ‘µ»¥˝ 10√Î....';
  Sleep(1000 * 10);
  ShowMessage('TaskProcGlobal invoke');
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogTask := TIocpTaskMananger.Create();
  FLogTask.IocpEngine.setWorkerCount(1);
  FLogTask.Active := true;
end;

destructor TfrmMain.Destroy;
begin
  iocpTaskManager.Enable := false;
  FLogTask.Active := false;
  FLogTask.Free;
  inherited Destroy;
end;

procedure TfrmMain.DoJobProc(pvTaskRequest: TIocpTaskRequest);
begin
  InterlockedIncrement(PInteger(pvTaskRequest.TaskData)^);
end;

procedure TfrmMain.logMessage(pvMsg: string);
begin
  FLogTask.PostATask(onLogMsg, pvMsg, True); 
end;

procedure TfrmMain.onLogMsg(pvStrData: string);
begin
  Memo1.Lines.Add(pvStrData);
end;

procedure TfrmMain.btnPostTaskClick(Sender: TObject);
begin
//  iocpTaskManager.PostATask(OnTaskWork, True, rtPostMessage);
//  iocpTaskManager.PostATask(OnTaskWork, True, rtSync);
//
//  iocpTaskManager.PostATask(OnTaskWork, '1');
//  iocpTaskManager.PostATask(OnTaskWork, '2');
//  iocpTaskManager.PostATask(OnTaskWork, '3');

  iocpTaskManager.PostATask(TaskProcGlobal, nil, False);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.Add(iocpTaskManager.getStateINfo());
end;

{ TfrmMain }

procedure TfrmMain.OnTaskWork(pvStrData:string);
begin
  Memo1.Lines.Add(pvStrData +': currentIsMainThread:' + BoolToStr(GetCurrentThreadId = MainThreadID, True));
end;

procedure TfrmMain.OnTaskWork;
var
  lvMsg:String;
begin
  lvMsg := 'currentIsMainThread:' + BoolToStr(GetCurrentThreadId = MainThreadID, True);
  logMessage(lvMsg);
end;



procedure TfrmMain.SpeedTesterClick(Sender: TObject);
const
  ACount:Integer=10000000;
var
  I,ARuns:Integer;
  T1:Int64;
  ANeedRuns:Int64;
begin
ARuns:=0;
ANeedRuns:=ACount;
T1:=GetTickCount;
for I := 0 to ACount-1 do
  begin
  iocpTaskManager.PostATask(DoJobProc,@ARuns);
  end;
while (ARuns<ANeedRuns) do
  {$IFDEF UNICODE}
  TThread.Yield;
  {$ELSE}
  SwitchToThread;
  {$ENDIF}
T1:=GetTickCount-T1;
ShowMessage('Time Used='+IntToStr(T1)+'ms,Runs='+IntToStr(ARuns)+
  ',Speed='+IntToStr(Int64(ARuns)*1000 div T1) + '/s'); 
end;

initialization



end.
