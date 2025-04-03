import express from "express";
import http from "http";
import { Server } from "socket.io";
import path from "path";
import axios from "axios";

const app = express();

const server = http.createServer(app);

const url = `https://codeconnect-psa3.onrender.com`;
const interval = 30000;

function reloadWebsite() {
  axios
    .get(url)
    .then((response) => {
      console.log("website reloded");
    })
    .catch((error) => {
      console.error(`Error : ${error.message}`);
    });
}

setInterval(reloadWebsite, interval);

const io = new Server(server, {
  cors: {
    origin: "*",
  },
  pingTimeout: 60000, // 60 seconds
  pingInterval: 25000, // 25 seconds
  connectTimeout: 45000, // 45 seconds
  maxHttpBufferSize: 1e8, // 100 MB
  transports: ['websocket', 'polling']
});

const rooms = new Map();

// Default code templates for different programming languages
const codeTemplates = {
  javascript: `// JavaScript Template
function main() {
    console.log("Hello, World!");
}

main();`,
  python: `# Python Template
def main():
    print("Hello, World!")

if __name__ == "__main__":
    main()`,
  java: `// Java Template
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}`,
  cpp: `// C++ Template
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}`,
  c: `// C Template
#include <stdio.h>

int main() {
    printf("Hello, World!\\n");
    return 0;
}`,
  csharp: `// C# Template
using System;

class Program {
    static void Main() {
        Console.WriteLine("Hello, World!");
    }
}`,
  go: `// Go Template
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}`,
  ruby: `# Ruby Template
def main
    puts "Hello, World!"
end

main`,
  rust: `// Rust Template
fn main() {
    println!("Hello, World!");
}`,
  php: `<?php
// PHP Template
function main() {
    echo "Hello, World!\\n";
}

main();
?>`,
  swift: `// Swift Template
func main() {
    print("Hello, World!")
}

main()`,
  kotlin: `// Kotlin Template
fun main() {
    println("Hello, World!")
}`,
  typescript: `// TypeScript Template
function main(): void {
    console.log("Hello, World!");
}

main();`,
  scala: `// Scala Template
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, World!")
  }
}`,
  r: `# R Template
main <- function() {
    print("Hello, World!")
}

main()`,
  perl: `# Perl Template
sub main {
    print "Hello, World!\\n";
}

main();`,
  haskell: `-- Haskell Template
main :: IO ()
main = putStrLn "Hello, World!"`,
  lua: `-- Lua Template
function main()
    print("Hello, World!")
end

main()`,
  dart: `// Dart Template
void main() {
    print('Hello, World!');
}`,
  elixir: `# Elixir Template
defmodule Main do
  def main do
    IO.puts("Hello, World!")
  end
end

Main.main()`,
  bash: `#!/bin/bash
# Bash Template
echo "Hello, World!"`,
  sql: `-- SQL Template
SELECT 'Hello, World!' AS message;`
};

io.on("connection", (socket) => {
  console.log("User Connected", socket.id);

  let currentRoom = null;
  let currentUser = null;

  socket.on("join", ({ roomId, userName }) => {
    if (currentRoom) {
      socket.leave(currentRoom);
      rooms.get(currentRoom).users.delete(currentUser);
      io.to(currentRoom).emit(
        "userJoined",
        Array.from(rooms.get(currentRoom).users)
      );
    }

    currentRoom = roomId;
    currentUser = userName;

    socket.join(roomId);

    if (!rooms.has(roomId)) {
      rooms.set(roomId, {
        users: new Set(),
        code: codeTemplates.javascript, // Default to JavaScript template for new rooms
        language: "javascript", // Default language for new rooms
      });
    }

    rooms.get(roomId).users.add(userName);

    // Send current code to the new user
    socket.emit("codeUpdate", rooms.get(roomId).code);

    // Send current room language to the new user
    socket.emit("languageUpdate", rooms.get(roomId).language);

    io.to(roomId).emit("userJoined", Array.from(rooms.get(currentRoom).users));
  });

  socket.on("codeChange", ({ roomId, code }) => {
    if (rooms.has(roomId)) {
      rooms.get(roomId).code = code;
    }
    socket.to(roomId).emit("codeUpdate", code);
  });

  socket.on("leaveRoom", () => {
    if (currentRoom && currentUser) {
      rooms.get(currentRoom).users.delete(currentUser);
      io.to(currentRoom).emit(
        "userJoined",
        Array.from(rooms.get(currentRoom).users)
      );

      socket.leave(currentRoom);

      currentRoom = null;
      currentUser = null;
    }
  });

  socket.on("typing", ({ roomId, userName }) => {
    socket.to(roomId).emit("userTyping", userName);
  });

  socket.on("languageChange", ({ roomId, language }) => {
    // Save the language setting for the room
    if (rooms.has(roomId)) {
      const room = rooms.get(roomId);
      room.language = language;
      room.code = codeTemplates[language] || codeTemplates.javascript;
    }
    io.to(roomId).emit("languageUpdate", language);
    io.to(roomId).emit("codeUpdate", codeTemplates[language] || codeTemplates.javascript);
  });

  socket.on(
    "compileCode",
    async ({ code, roomId, language, version, input }) => {
      try {
        if (!rooms.has(roomId)) {
          io.to(roomId).emit("compileError", {
            message: "Room not found. Please rejoin the room.",
          });
          return;
        }

        if (!code || !language) {
          io.to(roomId).emit("compileError", {
            message: "Invalid code or language selection.",
          });
          return;
        }

        const room = rooms.get(roomId);

        try {
          const response = await axios.post(
            "https://emkc.org/api/v2/piston/execute",
            {
              language,
              version: version || "*",
              files: [
                {
                  content: code,
                },
              ],
              stdin: input || "",
            },
            {
              timeout: 10000, // 10 second timeout
            }
          );

          if (response.data.error) {
            io.to(roomId).emit("compileError", {
              message: response.data.error || "Compilation failed",
            });
            return;
          }

          room.output = response.data.run.output;
          io.to(roomId).emit("codeResponse", response.data);
        } catch (apiError) {
          console.error("API Error:", apiError.message);
          io.to(roomId).emit("compileError", {
            message: "Code execution failed. Please try again.",
          });
        }
      } catch (error) {
        console.error("Server Error:", error.message);
        io.to(roomId).emit("compileError", {
          message: "Internal server error. Please try again.",
        });
      }
    }
  );

  socket.on("disconnect", () => {
    try {
      if (currentRoom && currentUser) {
        const room = rooms.get(currentRoom);
        if (room) {
          room.users.delete(currentUser);
          io.to(currentRoom).emit("userJoined", Array.from(room.users));
        }
      }
    } catch (error) {
      console.error("Disconnect Error:", error.message);
    }
    console.log("User Disconnected");
  });
});

const port = process.env.PORT || 5000;

const __dirname = path.resolve();

app.use(express.static(path.join(__dirname, "/frontend/dist")));

app.get("*", (req, res) => {
  res.sendFile(path.join(__dirname, "frontend", "dist", "index.html"));
});

server.listen(port, () => {
  console.log("server is working on port 5000");
});
