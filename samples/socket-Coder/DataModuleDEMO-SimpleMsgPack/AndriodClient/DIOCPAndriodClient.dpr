program DIOCPAndriodClient;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDTcpClientCoderImpl in '..\..\diocpCoders\uDTcpClientCoderImpl.pas',
  uICoderSocket in '..\..\diocpCoders\uICoderSocket.pas',
  uRemoteServerDIOCPImpl in 'service\uRemoteServerDIOCPImpl.pas',
  uIRemoteServer in 'interface\uIRemoteServer.pas',
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas',
  {$IFDEF POSIX}
  DZipTools in '..\..\diocpCoders\DZipTools.pas',
  {$ELSE}
  uZipTools in '..\..\diocpCoders\uZipTools.pas',
  {$ENDIF }
  uStreamCoderSocket in '..\..\diocpCoders\uStreamCoderSocket.pas',
  Datasnap.DBClient in 'Datasnap.DBClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
