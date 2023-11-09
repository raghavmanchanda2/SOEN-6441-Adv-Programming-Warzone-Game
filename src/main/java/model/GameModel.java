package model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;

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
	 * @return
	 */
		public Queue<Player> getPlayerQueue() {
			return playerQ;
		}

	/**
	 * method to add player in queue
	 * @param player
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
	 * @return
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
	//--------------------------------------------------------------------------------------------------



	
}
