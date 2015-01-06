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

    function ReadLn(pvSocketObj: TDRawSocket; const eol: AnsiString = #10; const
        pvTimeOut: Integer = 30000): String;
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

function TfrmMain.ReadLn(pvSocketObj: TDRawSocket; const eol: AnsiString = #10;
    const pvTimeOut: Integer = 30000): String;
var
  len: Integer;
  buf: array[0..511] of AnsiChar;
  lveolPtr: PAnsiChar;
  lvTimeOut:Integer;
begin
  Result := '';
  lveolPtr := nil;
  lvTimeOut := pvTimeOut;
  repeat
    len := pvSocketObj.PeekBuf(buf, sizeof(buf) - 1);
    if len > 0 then
    begin
      buf[len] := #0;
      lveolPtr := strpos(buf, PAnsiChar(eol));
      if lveolPtr <> nil then
        len := lveolPtr - buf + length(eol);
      pvSocketObj.RecvBuf(buf[0], len);
      if lveolPtr <> nil then
        len := len - length(eol);
      buf[len] := #0;
      Result := Result + buf;
    end else
    begin
      Sleep(20);
      Dec(lvTimeOut,20);
      if lvTimeOut < 0 then
      begin
        raise Exception.Create('ReadLn ReadTimeout');
      end;
    end;
  until (len < 1) or (lveolPtr <> nil);
end;

procedure TfrmMain.tmrRecvTimer(Sender: TObject);
begin
  if FUDPListen.Readable(100) > 0 then
  begin
    mmoRecv.Lines.Add(ReadLn(FUDPListen, #10#13));
  end;
end;

end.
