unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DRawSocket, StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    edtHost: TEdit;
    edtPort: TEdit;
    btnSend: TButton;
    Memo1: TMemo;
    mmoRecv: TMemo;
    edtListen: TEdit;
    btnListen: TButton;
    tmrRecv: TTimer;
    procedure btnListenClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure tmrRecvTimer(Sender: TObject);
  private
    FUDPClient: TDRawSocket;
    FUDPListen: TDRawSocket;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUDPClient := TDRawSocket.Create();
  FUDPClient.CreateUdpSocket();

  FUDPListen := TDRawSocket.Create();
  FUDPListen.CreateUdpSocket();
  FUDPListen.SetNonBlock(False);
end;

procedure TfrmMain.btnListenClick(Sender: TObject);
begin
  FUDPListen.bind('', StrToInt(edtListen.Text));
  tmrRecv.Enabled := True;
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
var
  s:String;
begin
  s := Memo1.Lines.Text;
  FUDPClient.SetConnectInfo(edtHost.Text, StrToInt(edtPort.Text));
  FUDPClient.SendBufTo(s[1], Length(s));
end;

procedure TfrmMain.tmrRecvTimer(Sender: TObject);
begin
  if FUDPListen.Readable(100) > 0 then
  begin
    mmoRecv.Lines.Add('гаЪ§Он');
  end;
end;

end.
