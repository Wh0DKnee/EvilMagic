import socket

def connect():
    conn = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    conn.connect(('127.0.0.1', 9999)) # ip and port running emacs
    return conn

if __name__ == '__main__':
    conn = connect()

    # now you send your desired elisps commands like this:
    conn.send(b'(next-line)')
    conn.send(b'(insert "foo bar")')
    conn.send(b'(message "do what you want")')
