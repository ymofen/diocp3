unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, uIOCPCentre, iocpTcpServer, ExtCtrls,
  ComObj, FileTransProtocol;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    pnlMonitor: TPanel;
    actPushMsg: TAction;
    edtMsg: TEdit;
    btnPushMsg: TButton;
    procedure actOpenExecute(Sender: TObject);
    procedure actPushMsgExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TIOCPConsole;
    procedure refreshState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnRecvObject(pvClientContext: TIOCPCoderClientContext; pvObject: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, uDIOCPStreamCoder;

{$R *.dfm}



constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TIOCPConsole.Create(Self);
  FTcpServer.createDataMonitor;
  FTcpServer.OnDataObjectReceived := OnRecvObject;

  // register decoder and encoder class
  FTcpServer.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
end;

destructor TfrmMain.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMain.OnRecvObject(pvClientContext: TIOCPCoderClientContext;
    pvObject: TObject);
var
  lvFileHead, lvResult:TFileHead;
  lvStream:TStream;
  lvFile:String;  
begin
  lvStream := TStream(pvObject);
  if lvStream.Size < SizeOf(TFileHead) then
  begin  // other data
    pvClientContext.writeObject(pvObject);
  end else
  begin
    lvStream.Read(Pointer(@lvFileHead)^, SizeOf(TFileHead));
    if lvFileHead.Flag <> FILE_TRANS_FLAG  then
    begin        // other data
      pvClientContext.writeObject(pvObject);
    end else
    begin
      ZeroMemory(@lvResult, SizeOf(TFilehead));
      lvResult.Flag := FILE_TRANS_FLAG;

      if lvFileHead.cmd = 1 then
      begin    // request
        lvResult.cmd := 2;  //response

        lvFile := ExtractFilePath(ParamStr(0)) + 'files\' + lvFileHead.FileName;
        if not FileExists(lvFile) then
        begin
          lvResult.cmd_result := 1;  // file not found
        end;


      end;
    end;

  end;
end;

procedure TfrmMain.refreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Action := actStop;
  end else
  begin
    btnOpen.Action := actOpen;
  end;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.Active := true;
  refreshState;
end;

procedure TfrmMain.actPushMsgExecute(Sender: TObject);
var
  lvList:TList;
  i: Integer;
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  lvList := TList.Create;
  try
    lvStream := TMemoryStream.Create;
    try
      s := edtMsg.Text;
      lvStream.Write(s[1], Length(s));

      // get all client context to List
      FTcpServer.getOnlineContextList(lvList);


      for i := 0 to lvList.Count-1 do
      begin
        //send stream object directly
        TIOCPCoderClientContext(lvList[i]).writeObject(lvStream);
      end;
    finally
      lvStream.Free;
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

end.
