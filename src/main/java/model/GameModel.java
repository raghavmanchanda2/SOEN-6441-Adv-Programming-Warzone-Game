package model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;

public class GameModel {
	private List<Player> players;
	private Player currentPlayer;
	private Map<Player,Boolean> commitState;
	private Queue<Player> playerQ;
	
	
	private static GameModel gameModel;
	private GameModel() {
		
	}
	
	public static GameModel getInstance() {
		
		if (Objects.isNull(gameModel)) {
			gameModel = new GameModel();
			
		}
		return gameModel;
	}
	
	public List<Player> getPlayers() {
		return players;
	}
	public void setPlayers(List<Player> players) {
		this.players = players;
	}
	public Player getCurrentPlayer() {
		if(currentPlayer == null) {
			currentPlayer = players.get(0);
		}
		return currentPlayer;
	}
	public void setCurrentPlayer(Player currentPlayer) {
		this.currentPlayer = currentPlayer;
	}
	
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
		
		public void addPlayerCard() {
			for(Player player : players) {
				if(player.getCanAddCard()) {
					player.addCard();
				}
			}
		}
		
		public void resetPeaceForAllPlayers() {
			for(Player player : players) {
				player.resetPeaceWith();
			}
		}
		
		public Queue<Player> getPlayerQueue() {
			return playerQ;
		}
		
		public void addPlayerQueue(Player player) {
			playerQ.add(player);
		}
		
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
		
		public void resetCommit() {
			for(Player player : players) {
				player.resetCommit();
			}
		}
		
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
