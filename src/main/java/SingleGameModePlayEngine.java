
import java.util.List;
import java.util.Map;

import business.MainPlayPhaseBusinessCommands;
import controller.MainPlayPhaseController;
import controller.SingleGameModePlayEngineController;
import logger.ConsoleWriter;
import logger.GeneralException;
import logger.LogEntryBuffer;
import model.*;

/**
 * Single game mode class to be implemented in the future
 * @author Kevin
 * @version build 1
 */
public class SingleGameModePlayEngine {
	
	
	private SingleGameModePlayEngineController singleGameModePlayEngineController;
	private MainPlayPhaseController  mainPlayPhaseController;
	private MainPlayPhaseBusinessCommands mainPlayPhaseBusinessCommands;
	private GameModel gameModel;
	private MapModel mapModel;

	private LogEntryBuffer d_logger;
	private ConsoleWriter d_consoleWriter;

	String l_Table = "- %-21s - %-16s - %-22s%n";
	String l_Columns = " %-16s  %-20s   %-22s%n";

	

	public SingleGameModePlayEngine() {
		singleGameModePlayEngineController = new SingleGameModePlayEngineController();
		mainPlayPhaseController = new MainPlayPhaseController();
		mainPlayPhaseBusinessCommands = new MainPlayPhaseBusinessCommands();
		gameModel = GameModel.getInstance();
		mapModel = MapModel.getInstance();
		d_logger = new LogEntryBuffer();
		d_consoleWriter = new ConsoleWriter();
		
	}
	
	private void printPlaySetupCommands() {
		// loadmap
		// showmap
		
		// addplayers
		// assigncountries


		System.out.println(" ");
		System.out.println("****************************************");
		System.out.println("************* GAME ENGINE **************");
		System.out.println("****************************************");
		System.out.println(" ");
		System.out.println("-> To load an existing map: loadmap filename(.map)");
		System.out.println("-> To show the map: showmap");
		System.out.println("-> To add a player to the game: gameplayer -add playername");
		System.out.println("-> To remove a player to the game: gameplayer -remove playername");
		System.out.println("-> To assign the countries to all the players: assigncountries");
		System.out.println("-> To continue to the StartUp Phase: continue");
		System.out.println("-> To exit the game: exit");
		System.out.println(" ");
		System.out.println("***** Input any command to proceed *****");
		System.out.println("****(Getting input from the user...)****");
		
		
		
		
		
	}
	private void printMainPlaySetupCommands() {
		//Reinforcement
		
		
		// attack
		System.out.println(" ");
		System.out.println("****************************************");
		System.out.println("************ ORDER CREATION ************");
		System.out.println("****************************************");
		System.out.println(" ");
		System.out.println("-> Deploy Order Command:  deploy countryID numarmies");
		System.out.println("-> Advance Order Command: advance countrynamefrom countynameto numarmies");
		System.out.println("-> Bomb Order Command: bomb countryID");
		System.out.println("-> Airlift Order Command: airlift sourcecountryID targetcountryID numarmies");
		System.out.println("-> Blockade Order Command: blockade countryID");
		System.out.println("-> Diplomacy Order Command: negotiate playerID");
		System.out.println("-> Commit Orders: commit");
		System.out.println("-> Exit Game: exit");
		System.out.println(" ");
		System.out.println("***** Input any command to proceed *****");
		System.out.println("****(Getting input from the user...)****");
		System.out.println(" ");

		//commit
		
		// who win
	}
	
	
	
	public ResponseWrapper startGamePlayMode() throws GeneralException {
		
		ResponseWrapper initialSetupResponse ; 
		
		// initial setup phase
		while(true) {
			this.printPlaySetupCommands();
			initialSetupResponse = singleGameModePlayEngineController.getPlaySetupCommandsFromUser();
			System.out.println(initialSetupResponse.getDescription());
			if(initialSetupResponse.getStatusValue() == 201) {
				System.out.println("Moving to GamePlay Phase and can't be go back in initial set up phase");
				break;
			}
		}
		
		// Reinforcement attack and fortification
		ResponseWrapper mainPlaySetUpResponse;
		while(true) {
			
			gameModel.resetPeaceForAllPlayers();
			gameModel.resetCommit();
			
			// do Reinforcements 
			mainPlayPhaseBusinessCommands.doReinforcements();
			
			for(Player player : gameModel.getPlayers()) {
				if(player.getCanAddCard()) {
					player.addCard();
					player.endTurnCardReset();
				}
			}
			
			while(true) {
				// get player's turn 
				this.printMainPlaySetupCommands();
				Player currentPlayer = gameModel.getNextPlayer();
				System.out.println("*****************************************************");
				System.out.println(" Current Player  !  Initial Assigned  !  Left Armies");
				System.out.println("*****************************************************");
				System.out.format(l_Table, currentPlayer.getPlayerName(), currentPlayer.getCurrentArmies(),  currentPlayer.getArmiesToIssue());
				System.out.println("*****************************************************");
				String country_title = "Country Name";
				String armies_title = "Country Armies";
				String neighbors_title = "Neighbors";
				System.out.format(l_Columns, country_title, armies_title, neighbors_title);
				System.out.format("*****************************************************%n");
				
				Map<Country, List<Country>> neighbors = this.mapModel.getBorders();
				for (Country l_Country : currentPlayer.getCountriesHold()) {
					if (neighbors.containsKey(l_Country)){
						System.out.format(l_Table, l_Country.getCountryId(), l_Country.getArmies(), this.getCountriesList(neighbors.get(l_Country)));
					}
				}
				System.out.format("*****************************************************\n");
				
				gameModel.printCardsListForCurrentPlayer();
				// ask for attack commands phase  with player
				mainPlaySetUpResponse = mainPlayPhaseController.getMainPlaySetUpCommandsFromUser(currentPlayer);
				System.out.println(mainPlaySetUpResponse.getDescription());


				
				
//				if (!gameModel.doNextPlayer()) {
//					// no next player do commit state
//					System.out.println("****************************************");
//					break;
//				}
				 
				if(gameModel.checkAllCommit()) {
					break;
				}
				
			}
			// in execution if player capture country he will get card
			// in execution if player going to win
			mainPlayPhaseBusinessCommands.endGame(mainPlaySetUpResponse);
			
			System.out.println(mainPlaySetUpResponse.getDescription());
			if(mainPlaySetUpResponse.getStatusValue() == 201) {
				System.out.println("Game Ends");
				break;
			}
			
			
		}
		
		return null;
			
	}

	public String getCountriesList(List<Country> countriesList) {
		String l_countList = "";
		StringBuilder stringBuilder = new StringBuilder();
		
		for (Country country : countriesList) {
			stringBuilder.append(country.getCountryId());
			stringBuilder.append("-");
		}
		l_countList = stringBuilder.toString();
		return l_countList.length() > 0 ? l_countList.substring(0, l_countList.length() - 1) : "";
	}

}
