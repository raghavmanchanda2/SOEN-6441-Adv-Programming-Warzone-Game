package model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class GameModel {
	private List<Player> players;
	private Player currentPlayer;
	private Map<Player,Boolean> commitState;
	
	
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
}
