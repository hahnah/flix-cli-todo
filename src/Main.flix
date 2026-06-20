use Sys.Console

def main(): Unit \ Console = 
    processCommand()


def processCommand(): Unit \ Console =
    let command: Option[String] = Console.readlnWith("👉 list | add | done | delete | exit ▶︎ ", input ->
        match input {
            case "list" => Ok("list")
            case "add" => Ok("add")
            case "done" => Ok("done")
            case "delete" => Ok("delete")
            case "exit" => Ok("exit")
            case _ => Err("Please enter valid command.")
        }
    );
    match command {
        case Some("exit") => Console.println("👋 Bye")
        case Some(cmd) => 
          Console.println("You entered: ${cmd}");
          processCommand()
        case None => ()
    }
