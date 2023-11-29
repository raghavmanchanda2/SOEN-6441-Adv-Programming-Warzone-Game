package model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Scanner;

import Strategy.AggressiveStrategy;
import Strategy.BenevolentStrategy;
import Strategy.CheaterStrategy;
import Strategy.HumanStrategy;
import Strategy.RandomStrategy;
import business.MainPlayPhaseBusinessCommands;
import controller.MainPlayPhaseController;

/**
 * GameModel class to define different objects in the game
 * @author Kevin
 * @author ishaanbajaj
 * @version build 2
 */
public class GameModel {
	/**
	 * List of players in the game
	 */
	private List<Player> players;
	/**
	 * Object of player class to get current player
	 */
	private Player currentPlayer;
	/**
	 * Map of players who have committed
	 */
	private Map<Player,Boolean> commitState;
	/**
	 * Queue of players
	 */
	private Queue<Player> playerQ;

	/**
	 * Object of GameModel class
	 */
	private static GameModel gameModel;

	private boolean permanentStrategy = false;

	/**
	 * Default constructor
	 */
	private GameModel() {

	}



	/**
	 * getInstance method
	 * @return gamemodel
	 */
	public static GameModel getInstance() {

		if (Objects.isNull(gameModel)) {
			gameModel = new GameModel();

		}
		return gameModel;
	}

	/**
	 * method to get list of players
	 * @return players
	 */
	public List<Player> getPlayers() {
		return players;
	}

	/**
	 * method to set players in the list
	 * @param players list of players
	 */
	public void setPlayers(List<Player> players) {
		this.players = players;
	}

	/**
	 * method to get current player
	 * @return player
	 */
	public Player getCurrentPlayer() {
		if(currentPlayer == null) {
			currentPlayer = players.get(0);
		}
		return currentPlayer;
	}

	/**
	 * method to set current player
	 * @param currentPlayer player
	 */
	public void setCurrentPlayer(Player currentPlayer) {
		this.currentPlayer = currentPlayer;
	}

	/**
	 * method to add players in the list of players
	 * @param player player
	 */
	public void addPlayerInPlayersList(Player player) {

		if(this.players == null) {
			this.players = new ArrayList<>();
		}
		if(this.commitState == null) {
			this.commitState = new HashMap<>();
		}
		if(this.playerQ == null) {
			this.playerQ = new LinkedList<>();
		}
		this.players.add(player);
		this.commitState.put(player, false);
	}

	/**
	 * method to get next player if exists
	 * @return boolean if next player exists
	 */
	public boolean doNextPlayer() {
		if(currentPlayer == null) {
			currentPlayer = players.get(0);
		}else {
			for(Player player : players) {
				if(currentPlayer.equals(player) &&  !commitState.get(player)) {
					int indexToAdd = players.indexOf(player) + 1 == players.size() ? 0 : 1;

					currentPlayer = players.get(indexToAdd == 0 ? 0 :players.indexOf(player)+indexToAdd);
					return true;
				}
			}
		}
		return false;

	}

	//--------------------------------------------------------------------------------------------------

	/**
	 * method to print cards list for current player
	 */
	public void printCardsListForCurrentPlayer() {
		int card_num = 1;
		System.out.println("CARDS CURRENTLY OWNED BY: " + currentPlayer.getPlayerName());
		System.out.println("*****************************************************");

		for(Card card : currentPlayer.getCardList()) {
			System.out.println(card_num + ". " + card.getCardType().name());
			++card_num;
		}
		System.out.println("*****************************************************");
	}

	/**
	 * method to add player card
	 */
	public void addPlayerCard() {
		for(Player player : players) {
			if(player.getCanAddCard()) {
				player.addCard();
			}
		}
	}

	/**
	 * method to reset peace for all players
	 */
	public void resetPeaceForAllPlayers() {
		for(Player player : players) {
			player.resetPeaceWith();
		}
	}

	/**
	 * method to get queue of players
	 * @return player player in queue
	 */
	public Queue<Player> getPlayerQueue() {
		return playerQ;
	}

	/**
	 * method to add player in queue
	 * @param player player
	 */
	public void addPlayerQueue(Player player) {
		playerQ.add(player);
	}

	/**
	 * method to get next player
	 * @return player
	 */
	public Player getNextPlayer() {
		Player next = null;
		Player put_back = null;
		boolean found = false;
		while(!found) {
			next = playerQ.poll();
			put_back = next;
			if(next.getCommit()) {
				playerQ.add(next);
			}
			else {
				found = true;
			}
		}
		currentPlayer = next;
		playerQ.add(put_back);
		return next;
	}

	/**
	 * method to reset commit state of players
	 */
	public void resetCommit() {
		for(Player player : players) {
			player.resetCommit();
		}
	}

	/**
	 * method to check all commit state players
	 * @return allcommit returning commit state
	 */
	public boolean checkAllCommit() {
		boolean allCommit = true;
		for(Player player : players) {
			if(player.getCommit() == false) {
				allCommit = false;
			}
		}
		return allCommit;
	}


/**
 *  To edit the strategy  by getting from user
 * @param p_mainPlayPhaseController main play phase controller
 */
	public void editStrategy(MainPlayPhaseController p_mainPlayPhaseController) {
		Scanner scanner = new Scanner(System.in);

		boolean found = false;


		System.out.println("DO YOU WANT TO EDIT A PLAYER STRATEGY? Press 1 for yes or 0 for no or 2 to permanently set the strategies: ");
		int userInput = scanner.nextInt();

		if(userInput == 1) {

			do {
				printPlayerStrategies();
				System.out.print("Enter the name of the player you want to edit or enter exit: ");
				String input = scanner.next();

				if(input.equalsIgnoreCase("exit")) {
					scanner.close();
					break;
				}
				else {
					for(Player player : players) {
						if(player.getPlayerName().equalsIgnoreCase(input)) {
							found = changeStrat(player, p_mainPlayPhaseController);
							break;
						}
					}
				}

				if(!found) {
					System.out.println("Player Not Found - Please Enter Correct Name");
				}

			}
			while(true);

		}
		else if(userInput == 2) {
			permanentStrategy = true;
		}


	}
/**
 *  to check if the strategy is permanent
 * @return the boolean value of the strategy
 */
	public boolean isStrategyPermanent() {
		return permanentStrategy;
	}

	/**
	 * To change the strategy of the player
	 * @param player the player whose strategy going to be changes
	 * @param p_mainPlayPhaseController the controller main play phase commands
	 * @return the boolean value to change the strategy
	 */
	public boolean changeStrat(Player player, MainPlayPhaseController p_mainPlayPhaseController) {

		Scanner scanner = new Scanner(System.in);

		while(true)
		{
			System.out.println("Choose which strategy to set for the following player: " + player.getPlayerName());

			System.out.println("------------------------------------------------------------------------");

			System.out.println("1. Human Strategy");
			System.out.println("2. Aggressive Strategy");
			System.out.println("3. Benevolent Strategy");
			System.out.println("4. Random Strategy");
			System.out.println("5. Cheater Strategy");
			System.out.println("6. Exit");

			System.out.print("Enter a number between 1 to 6: ");
			int value = scanner.nextInt();

			switch(value) {

				case 1:
					player.setStrategy(new HumanStrategy(player, MapModel.getInstance(), p_mainPlayPhaseController, MainPlayPhaseBusinessCommands.getMainPlayPhaseBusinessCommandsInstance()));
					return true;

				case 2:
					player.setStrategy(new AggressiveStrategy(player, MapModel.getInstance(), p_mainPlayPhaseController, MainPlayPhaseBusinessCommands.getMainPlayPhaseBusinessCommandsInstance()));
					return true;

				case 3:
					player.setStrategy(new BenevolentStrategy(player, MapModel.getInstance(), p_mainPlayPhaseController, MainPlayPhaseBusinessCommands.getMainPlayPhaseBusinessCommandsInstance()));
					return true;

				case 4:
					player.setStrategy(new RandomStrategy(player, MapModel.getInstance(), p_mainPlayPhaseController, MainPlayPhaseBusinessCommands.getMainPlayPhaseBusinessCommandsInstance()));
					return true;

				case 5:
					player.setStrategy(new CheaterStrategy(player, MapModel.getInstance(), p_mainPlayPhaseController, MainPlayPhaseBusinessCommands.getMainPlayPhaseBusinessCommandsInstance()));
					return true;

				case 6:
					return true;

				default:
					System.out.println("INVALID INPUT");

			}
		}



	}
	/**
	 * TO PRINT PLAYER STRATEGIES
	 */
	public void printPlayerStrategies() {
		String l_Columns = "%-21s   %-22s%n";

		System.out.println("CURRENT PLAYER STRATEGIES");
		System.out.println("-------------------------");

		for(Player player : players) {
			System.out.format(l_Columns, player.getPlayerName(), player.getStrategy().getStrategyName());
		}
	}

	//--------------------------------------------------------------------------------------------------
































}
