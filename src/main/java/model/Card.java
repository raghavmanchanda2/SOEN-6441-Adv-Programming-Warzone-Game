package model;
import java.util.*;

public class Card
{
	public enum CardType
	{
		BOMB(1), BLOCKADE(2), AIRLIFT(3), DIPLOMACY(4);
		
		private final int cardID;
	
		CardType(int i)
		{
			cardID = i;
		}
		
		public int getID()
		{
			return cardID;
		}
	}
	
	private CardType cardType;
	
	public Card(CardType cardType)
	{
		this.cardType = cardType;
	}
	
	public CardType getCardType()
	{
		return cardType;
	}
	
	
	public static Card generateRandomCard()
	{
		Random random = new Random();
			
		int randomID = random.nextInt(4) + 1;
			
		CardType randomCard = null;
			
			for(CardType cardType : CardType.values())
			{
				if(cardType.getID() == randomID)
				{
					randomCard = cardType;
					break;
				}
			}
			
			if(randomCard == null)
			{
				throw new IllegalArgumentException("Trying to insert null");
			}
		
		return new Card(randomCard);
	}
}