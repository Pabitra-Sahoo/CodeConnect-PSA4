// Default code templates for different programming languages
export const codeTemplates = {
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