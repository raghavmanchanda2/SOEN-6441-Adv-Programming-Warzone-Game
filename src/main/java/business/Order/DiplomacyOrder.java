package business.Order;

import model.Player;

public class DiplomacyOrder implements Order{
	
	Player currentPlayer, peaceWith;
	
	public DiplomacyOrder(Player currentPlayer, Player peaceWith) {
		this.currentPlayer = currentPlayer;
		this.peaceWith = peaceWith;
	}

	@Override
	public void execute() {
		currentPlayer.setPeaceWith(peaceWith);
		peaceWith.setPeaceWith(currentPlayer);
	}

	@Override
	public boolean valid() {

		if(peaceWith == null) {
			return false;
		}
		else {
			return true;
		}
		
	}

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}
