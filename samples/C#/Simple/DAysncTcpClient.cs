using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net.Sockets;
using System.Collections;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;


namespace DIOCPClient
{
    public class StateObject
    {
        public Socket workSocket = null;
        public const int BUFFER_SIZE = 1024;
        public byte[] buffer = new byte[BUFFER_SIZE];
        public StringBuilder sb = new StringBuilder();
    }

    public class DAysncTcpClient
    {
        public delegate void TcpClientEvent(object sender);
        public delegate void SocketRecvBufferEvent(byte[] buf, int len);

        public event TcpClientEvent OnConnected;

        public event TcpClientEvent OnDisconnected;

        public event SocketRecvBufferEvent OnRecvBuffer;
        
        private TcpClient rawSocket = new TcpClient();

        private void InnerDoRecvBuffer(byte[] buf, int len)
        {
            if (OnRecvBuffer != null)
            {
                OnRecvBuffer(buf, len);
            }            
        }

        private void InnerPostRecv()
        {
            StateObject obj = new StateObject();
            obj.workSocket = rawSocket.Client;
            rawSocket.Client.BeginReceive(obj.buffer, 0, StateObject.BUFFER_SIZE, 0,
                           new AsyncCallback(Read_Callback), obj);
        }


         private void InnerDoConnected()
        {
            if (OnConnected != null)
            {
                OnConnected(this);                
            }
            InnerPostRecv();
        }

        private void InnerDoDisconnected()
        {
            this.rawSocket.Client.Disconnect(true);
            if (OnDisconnected != null)
            {
                OnDisconnected(this);
            }
        }

        /// <summary>
        ///   接受数据异步事件
        /// </summary>
        /// <param name="ar"></param>
        void Read_Callback(IAsyncResult ar)
        {
            StateObject so = (StateObject)ar.AsyncState;
            Socket s = so.workSocket;


            int len = s.EndReceive(ar);

            if (len > 0)
            {
                InnerDoRecvBuffer(so.buffer, len);

                InnerPostRecv();
            }
            else
            {
                InnerDoDisconnected();
            }
        }

        /// <summary>
        ///   发送异步事件
        /// </summary>
        /// <param name="ar"></param>
        void Send_Callback(IAsyncResult ar)
        {
            int len = this.rawSocket.Client.EndSend(ar);

            if (len > 0)
            {
                
            }
            else
            {
                InnerDoDisconnected();
            }
        }
        

        private void InnerOnAsyncConnected(IAsyncResult result)
        {
            if (rawSocket.Connected)
            {
                rawSocket.Client.EndConnect(result);
                // 触发连接事件
                InnerDoConnected();
            }else
            {
                // 触发断开事件
                InnerDoDisconnected();
            }
        }

        private String host;

        public String Host
        {
            get { return host; }
            set { host = value; }
        }

        
        private int port;

        public int Port
        {
            get { return port; }
            set { port = value; }
        }

        /// <summary>
        ///   开始同步连接
        /// </summary>
        public void Connect()
        {           
            rawSocket.Connect(host, port);

            // 触发连接事件
            OnConnected(this);  
        }

        /// <summary>
        ///   开始异步连接
        /// </summary>
        public void ConnectASync()
        {            
            rawSocket.BeginConnect(host, port, new AsyncCallback(InnerOnAsyncConnected), rawSocket);
        }
        
        public void PostASyncBuffer(byte[] buf, int len)
        {
            rawSocket.Client.BeginSend(buf, 0, len, 0,new AsyncCallback(Send_Callback), rawSocket);
        }

    }
}
