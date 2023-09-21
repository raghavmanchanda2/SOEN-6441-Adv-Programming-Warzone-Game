package controller;

import java.util.Scanner;

import model.Continent;

public class MapEngineController {

	private Scanner inputForMapCommands;

	enum MapCommands {
		EDIT_CONTINENT("editcontinent"), EDIT_COUNTRY("editcountry"), EDIT_NEIGHBOR("editneighbor"),
		SHOW_MAP("showmap"), SAVE_MAP("savemap"), EDIT_MAP("editmap"), VALIDATE_MAP("validatemap"), NONE("nothing");

		public final String exactCommand;

		MapCommands(String exactCommand) {
			this.exactCommand = exactCommand;
		}
	}

	public MapEngineController() {
		inputForMapCommands = new Scanner(System.in);
	}

	public MapCommands getMapCommandsFromUser() {

		String userEnteredCommand = inputForMapCommands.nextLine();
		if (userEnteredCommand.isEmpty()) {
			return MapCommands.NONE;
		}
		// Filter
		String[] splittedCommands = userEnteredCommand.trim().replaceAll(" +", " ").split("\\s+");

		switch (splittedCommands[0]) {

		case "editcontinent":
			if(splittedCommands.length == 4 ) {
				Continent continent = new Continent(splittedCommands[3],splittedCommands[4]);
				switch (splittedCommands[1]) {
					
					case "-add":
						
						// call business file to execute command
						
						break;
						
						
					case "-remove":
						// call business file to execute command
						
						break;
						
					default:
						return MapCommands.NONE;
				}
			
			}else {
				return MapCommands.NONE;
			}
			
			
			break;
			
			

		case "editcountry":
			switch (splittedCommands[1]) {
			case "-add":
				break;
			case "-remove":
				break;
			}
			break;
		case "editneighbor":
			switch (splittedCommands[0]) {
			case "-add":
				break;
			case "-remove":
				break;
			}
			break;
		case "showmap":
			
			
			break;
		case "savemap":
			break;
		case "editmap":
			break;
		case "validatemap":
			break;
		default:
			return MapCommands.NONE;

		}

		return MapCommands.NONE;
	}

}
