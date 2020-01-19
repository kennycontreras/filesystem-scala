package commands
import files.{DirEntry, Directory}
import filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(name + " must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String) : State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      /*
      someDir
        /a
        /b
        (new)
        => new someDir
        /a
        /b
        /d
       */
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        println(path)
        println(currentDirectory.findEntry(path.head))
        /*
          currentDirectory = /a
          path = ["b"]
         */
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))

        /*
          /a/b
            (content)
            (new entry) /e

           updateStructure(root, ["a","b"], /e)
            => path.isEmpty ?
            => oldEntruy = /a
            root.replaceEntry("a", updateStructure(/a, [b], /e)
              => path.isEmpty ?
              /a.replaceEntry("a", updateStructure(/b, [], /e)
                => path.isEmpty ? => /b.add(/e)
         */
      }
    }

    val wd = state.wd

    // 1. all the directories in the full path
    val allDirsPath = wd.getAllFoldersInPath

    // 2. create new directory in the wd
    val newDir = Directory.empty(wd.path, name)

    // 3. update the whole directory structure starting from tbe root
    // the directory structure is immutable.
    val newRoot = updateStructure(state.root, allDirsPath, newDir)

    // 4. find the new working directory INSTANCE given wd's full path, in the NEW directory structure
    val newWd = newRoot.findDescendant(allDirsPath)
    State(newRoot, newWd)
  }
}
