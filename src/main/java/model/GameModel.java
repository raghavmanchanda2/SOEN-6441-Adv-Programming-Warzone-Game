package model;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class GameModel {
	private List<Player> players;
	private Player currentPlayer;
	
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
		return currentPlayer;
	}
	public void setCurrentPlayer(Player currentPlayer) {
		this.currentPlayer = currentPlayer;
	}
	
	public void addPlayerInPlayersList(Player player) {
		
		if(this.players == null) {
			this.players = new ArrayList<>();
		}
		this.players.add(player);
	}
	
	public Player getNextPlayer() {
		if(currentPlayer == null) {
			currentPlayer = players.get(0);
		}else {
			for(Player player : players) {
				if(currentPlayer == player) {
					int indexToAdd = players.indexOf(player) + 1 == players.size() ? 0 : 1;
					currentPlayer = players.get(players.indexOf(player)+indexToAdd);
				}
			}
		}
		return currentPlayer;
	}
	
	


	
}
