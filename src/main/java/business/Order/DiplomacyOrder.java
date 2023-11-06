package business.Order;

import model.Card;
import model.Player;

public class DiplomacyOrder implements Order{
	
	Player currentPlayer;
	Player peaceWith;
	
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
		
		boolean hasCard = false;
		
		for(Card card : currentPlayer.getCardList()) {
			if(card.getCardType() == Card.CardType.DIPLOMACY) {
				hasCard = true;
			}
		}
		if(!hasCard) {
			return false;
		}
		
		if(peaceWith != null) {
			
			for(Card card : currentPlayer.getCardList()) {
				if(card.getCardType() == Card.CardType.DIPLOMACY) {
					currentPlayer.getCardList().remove(card);
					break;
				}
			}
			
			return true;
		}
		
		return false;
	}

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}
