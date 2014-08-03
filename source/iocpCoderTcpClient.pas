unit iocpCoderTcpClient;

interface

uses
  iocpTcpClient, uIocpCoder, uBuffer, SysUtils, Classes;

type

  TOnDataObjectReceived = procedure(pvObject:TObject) of object;


  TiocpCoderTcpClient = class(TIocpTcpClient)
  private
    FRecvBufferLink: TBufferLink;

    FInnerEncoder: TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;

    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
    FOnDataObjectReceived: TOnDataObjectReceived;
  protected
    /// <summary>
    ///   on recved data, run in iocp worker thread
    /// </summary>
    procedure DoRecvd(buf: Pointer; len: Cardinal; errCode: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure registerCoderClass(pvDecoderClass:TIOCPDecoderClass;
        pvEncoderClass:TIOCPEncoderClass);
    /// <summary>
    ///   注册解码器
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure registerDecoder(pvDecoder:TIOCPDecoder);

    /// <summary>
    ///   注册编码器
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure registerEncoder(pvEncoder:TIOCPEncoder);


    /// <summary>
    ///   发送一个对象到服务端
    /// </summary>
    procedure writeObject(pvObject:TObject);

  published

    /// <summary>
    ///   接收到一个对象
    /// </summary>
    property OnDataObjectReceived: TOnDataObjectReceived read FOnDataObjectReceived
        write FOnDataObjectReceived;


  end;



implementation

uses
  uIOCPFileLogger;


//
//
//procedure TIOCPClientContext.writeObject(const pvDataObject:TObject);
//var
//  lvOutBuffer:TBufferLink;
//  lvRequest:TIocpSendRequest;
//begin
//  lvOutBuffer := TBufferLink.Create;
//  try
//    TiocpCoderTcpClient(Owner).FEncoder.Encode(pvDataObject, lvOutBuffer);
//  except
//    lvOutBuffer.Free;
//    raise;
//  end;
//
//  lvRequest := TIocpSendRequest(getSendRequest);
//  lvRequest.setBufferLink(lvOutBuffer);
//
//
//  postSendRequest(lvRequest);
//
//  self.StateINfo := 'TIOCPClientContext.writeObject,投递到发送缓存';
//
//end;

constructor TiocpCoderTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecvBufferLink := TBufferLink.Create();
end;

destructor TiocpCoderTcpClient.Destroy;
begin
  FreeAndNil(FRecvBufferLink);
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  inherited Destroy;
end;

procedure TiocpCoderTcpClient.DoRecvd(buf: Pointer; len: Cardinal; errCode:
    Integer);
var
  lvObject:TObject;
begin
  inherited;
  FRecvBufferLink.AddBuffer(buf, len);

  while True do
  begin
    //调用注册的解码器<进行解码>
    lvObject := FDecoder.Decode(FRecvBufferLink);
    if Integer(lvObject) = -1 then
    begin
      self.Disconnect;
      exit;
    end else if lvObject <> nil then
    begin
      try
        try
          if Assigned(FOnDataObjectReceived) then
            FOnDataObjectReceived(lvObject);
        except
          on E:Exception do
          begin
            TIOCPFileLogger.logErrMessage('截获处理逻辑异常!' + e.Message);
          end;
        end;
      finally
        lvObject.Free;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;

  //清理缓存<如果没有可用的内存块>清理
  if FRecvBufferLink.validCount = 0 then
  begin
    FRecvBufferLink.clearBuffer;
  end else
  begin
    FRecvBufferLink.clearHaveReadBuffer;
  end;
end;

procedure TiocpCoderTcpClient.registerCoderClass(pvDecoderClass: TIOCPDecoderClass;
    pvEncoderClass: TIOCPEncoderClass);
begin
  if FInnerDecoder <> nil then
  begin
    raise Exception.Create('已经注册了解码器类');
  end;

  FInnerDecoder := pvDecoderClass.Create;
  registerDecoder(FInnerDecoder);

  if FInnerEncoder <> nil then
  begin
    raise Exception.Create('已经注册了编码器类');
  end;
  FInnerEncoder := pvEncoderClass.Create;
  registerEncoder(FInnerEncoder);
end;

{ TiocpCoderTcpClient }

procedure TiocpCoderTcpClient.registerDecoder(pvDecoder: TIOCPDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TiocpCoderTcpClient.registerEncoder(pvEncoder: TIOCPEncoder);
begin
  FEncoder := pvEncoder;
end;


procedure TiocpCoderTcpClient.writeObject(pvObject: TObject);
var
  lvBufLink:TBufferLink;
begin
  lvBufLink := TBufferLink.Create;
  try
    FEncoder.Encode(pvObject, lvBufLink);

    //lvBufLink.readBuffer()

  finally
    lvBufLink.Free;
  end;

end;

end.
