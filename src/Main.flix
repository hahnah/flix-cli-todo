use Sys.Console

type alias Todo = { description = String, done = Bool }

def main(): Unit \ Console =
    let todoList: List[Todo] = Nil;
    processCommand(todoList)

def processCommand(todoList: List[Todo]): Unit \ Console =
    showTodoList(List.reverse(todoList));
    let command: Option[String] = Console.readlnWith("👉 add | done | delete | exit ▶︎ ", input ->
        match input {
            case "add"    => Ok("add")
            case "done"   => Ok("done")
            case "delete" => Ok("delete")
            case "exit"   => Ok("exit")
            case _        => Err("Please enter valid command.")
        }
    );
    match command {
        case Some("add") =>
            let description: Option[String] = Console.readlnWith("Enter description ▶︎ ", input -> Ok(input));
            match description {
                case Some(desc) => desc |> addTodo(todoList) |> processCommand
                case None => processCommand(todoList)
            }
        case Some("done") =>
            let donePosition: Option[Int32] = Console.readlnWith("Enter done number ▶︎ ", input ->
                match Int32.fromString(input) {
                    case Some(n) => Ok(n)
                    case None    => Err("Please enter a number.")
                }
            );
            match donePosition {
                case Some(position) => position |> doneTodo(todoList) |> processCommand
                case None => processCommand(todoList)
            }
        case Some("exit") => Console.println("👋 Bye")
        case Some(cmd) =>
            Console.println("You entered: ${cmd}");
            processCommand(todoList)
        case None => ()
    }

def showTodoList(todoList: List[Todo]): Unit \ Console =
    if (List.isEmpty(todoList)) {
        Console.println("Empty Todo List...")
    } else {
        showTodoListHelper(todoList, 1)
    }

def showTodoListHelper(todoList: List[Todo], index: Int32): Unit \ Console =
    match todoList {
        case Nil => ()
        case item :: rest =>
            let doneStr = if (item#done) "[x]" else "[ ]";
            Console.println("${index}.${doneStr} ${item#description}");
            showTodoListHelper(rest, index + 1)
    }

def addTodo(todoList: List[Todo], description: String): List[Todo] =
    let newTodo: Todo = { description = description, done = false };
    newTodo :: todoList


def doneTodo(todoList: List[Todo], donePosition: Int32): List[Todo] =
    todoList
        |> List.reverse
        |> List.mapWithIndex((index, item) -> if (index + 1 == donePosition) { description = item#description, done = true } else item)
        |> List.reverse
