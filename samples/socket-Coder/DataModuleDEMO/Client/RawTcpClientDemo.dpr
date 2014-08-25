program RawTcpClientDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uICoderSocket in '..\..\diocpCoders\uICoderSocket.pas',
  uStreamCoderSocket in '..\..\diocpCoders\uStreamCoderSocket.pas',
  uRawTcpClientCoderImpl in '..\..\diocpCoders\uRawTcpClientCoderImpl.pas',
  uIRemoteServer in 'interface\uIRemoteServer.pas',
  uRemoteServerDIOCPImpl in 'service\uRemoteServerDIOCPImpl.pas',
  pcre in '..\..\..\qdac3-source\pcre.pas',
  PerlRegEx in '..\..\..\qdac3-source\PerlRegEx.pas',
  qmsgpack in '..\..\..\qdac3-source\qmsgpack.pas',
  qstring in '..\..\..\qdac3-source\qstring.pas',
  qworker in '..\..\..\qdac3-source\qworker.pas',
  qrbtree in '..\..\..\qdac3-source\qrbtree.pas',
  uZipTools in '..\..\..\Common\uZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
