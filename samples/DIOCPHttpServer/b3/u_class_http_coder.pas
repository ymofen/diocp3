{ ******************************************************* }
{ }
{ u_class_http_packet }
{ Create By： Locet 2014/8/28 }
{ }
{ ******************************************************* }
unit u_class_http_coder;

interface

uses
  uIocpCoder, uBuffer, Classes, SysUtils, iocpTcpServer;

type

  THttpDecoder = class(TIOCPDecoder)
  public
    /// <summary>
    /// 解码收到的数据,如果有接收到数据,调用该方法,进行解码
    /// </summary>
    /// <returns>
    /// 返回解码好的对象
    /// </returns>
    /// <param name="inBuf"> 接收到的流数据 </param>
    function Decode(const inBuf: TBufferLink; pvContext: TObject)
      : TObject; override;
  end;

  THttpEncoder = class(TIOCPEncoder)
  private
  public
    /// <summary>
    /// 编码要发生的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="ouBuf"> 编码好的数据 </param>
    procedure Encode(pvDataObject: TObject; const ouBuf: TBufferLink); override;
  end;

implementation

{ THttpDecoder }

uses u_class_http_packet, System.StrUtils;

function THttpDecoder.Decode(const inBuf: TBufferLink;
  pvContext: TObject): TObject;
var
  ch: Char;
  pch: PAnsiChar;
  len: Integer;
  CR, LF: Integer;
  client: THttpClientContext;
  sa: RawByteString;
  request: THttpRequest;
begin
  // 在这里解析客户端浏览器发送过来的请求数据
  client := THttpClientContext(pvContext);
  if client.client_state_ = csDone then
    client.Reset;
  pch := @ch;
  Result := nil;
  len := inBuf.validCount;
  CR := 0;
  LF := 0;
  while (len > 0) do
  begin

    if (client.client_state_ = csRequest) then
    begin
      inBuf.readBuffer(pch, 1);
      case pch^ of
        #13:
          Inc(CR);
        #10:
          Inc(LF);
      else
        CR := 0;
        LF := 0;
      end;
      request := client.CreateHttpPacket();

      // 写入请求数据
      request.Write(pch^, 1);

      // 如果不是有效的Http请求直接断开
      if (request.Size > 7) and (not request.IsValidHttpRequest()) then
      begin
        Result := TObject(-1);
        Exit;
      end;

      // 请求数据已接收完毕(#13#10#13#10是HTTP请求结束的标志)
      if (CR = 2) and (LF = 2) then
      begin
        if not ParseRequestData(request) then
        begin
          Result := TObject(-1);
          Exit;
        end;

        if SameText(request.method_, 'POST') or SameText(request.method_, 'PUT')
        then
        begin
          // 无效的Post请求直接断开
          if (request.request_content_length_ <= 0) then
          begin
            Result := TObject(-1);
            Exit;
          end;
          // packet.FRequestPostData.Size := 0;
          // packet.FPostDataSize := 0;
          client.client_state_ := csPostData;
          request.is_accept_post_data_ := True;
          // TriggerAcceptPostData(packet.FRequestContentLength,
          // packet.FAcceptPostData);
        end
        else
        begin
          client.client_state_ := csDone;
          Break;
        end;
      end;

      Dec(len);
    end
    else if (client.client_state_ = csPostData) then
    begin
      request := client.CreateHttpPacket();
      Inc(request.post_data_size_, len);
      if request.is_accept_post_data_ then
      begin
        SetLength(sa, len);
        inBuf.readBuffer(PAnsiChar(@sa[1]), len);
        request.WritePostData(sa[1], len);
      end;

      if (request.post_data_size_ >= request.request_content_length_) then
        client.client_state_ := csDone;

      // Post数据直接剩余部分整段处理，到这里就已经全部处理完了，直接跳出循环
      Break;
    end;
  end;

  // 在解析完请求数据之后再调用线程池
  if (client.client_state_ = csDone) then
  begin
    Result := client.GetData;
  end;
end;

{ THttpEncoder }

function FixHeader(const Header: string): string;
begin
  Result := Header;
  if (RightStr(Header, 4) <> #13#10#13#10) then
  begin
    if (RightStr(Header, 2) = #13#10) then
      Result := Result + #13#10
    else
      Result := Result + #13#10#13#10;
  end;
end;

function MakeHeader(const Status, ContType, Header: string;
  ContSize: Integer): string;
begin
  Result := '';

  if (Status = '') then
    Result := Result + 'HTTP/1.1 200 OK' + #13#10
  else
    Result := Result + 'HTTP/1.1 ' + Status + #13#10;

  if (ContType = '') then
    Result := Result + 'Content-Type: text/html' + #13#10
  else
    Result := Result + 'Content-Type: ' + ContType + #13#10;

  if (ContSize > 0) then
    Result := Result + 'Content-Length: ' + IntToStr(ContSize) + #13#10;
  // Result := Result + 'Cache-Control: no-cache'#13#10;

  // if FKeepAlive then
  // Result := Result + 'Connection: keep-alive'#13#10
  // else
  Result := Result + 'Connection: close'#13#10;

  // if (IOCP_HTTP_SERVER_VERSION <> '') then
  Result := Result + 'Server: DIOCP3_HTTP/1.0' + #13#10;

  if (Header <> '') then
    Result := Result + FixHeader(Header)
  else
    Result := Result + #13#10;
end;

procedure THttpEncoder.Encode(pvDataObject: TObject; const ouBuf: TBufferLink);
var
  sHead: string;
  FixedHeader: AnsiString;
  resuponse: THttpResponse;
  len: Integer;
begin
  if pvDataObject = nil then
    Exit;
  resuponse := THttpResponse(pvDataObject);
  sHead := MakeHeader(resuponse.status_, resuponse.cont_type_,
    resuponse.header_, resuponse.cont_size);
  if (sHead <> '') then
  begin
    FixedHeader := RawByteString(FixHeader(sHead));
    len := Length(FixedHeader);
    ouBuf.AddBuffer(@FixedHeader[1], len);
    // FResponseSize := len + Size;
  end
  else
  begin
    // FResponseSize := Size;
  end;
  if (resuponse.data_ <> nil) and (resuponse.cont_size > 0) then
    ouBuf.AddBuffer(resuponse.data_, resuponse.cont_size);
end;

end.
