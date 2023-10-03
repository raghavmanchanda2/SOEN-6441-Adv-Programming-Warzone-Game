package controller;

import java.util.InputMismatchException;
import java.util.Scanner;

public class WarzoneEngineController {
	
	private Scanner d_inputForFeatureSelection;
	
	
	public int getGameFeatureInputs() {
		int l_selectedFeature = 404;
		try {
			
			d_inputForFeatureSelection = new Scanner(System.in);
			l_selectedFeature = d_inputForFeatureSelection.nextInt();
			
		
		}catch(InputMismatchException p_inputMismatchException){
			// throws global exception
			System.out.println("Please provide proper input");
			return 404;
		}
		
		if (l_selectedFeature > 0  && l_selectedFeature <= 4)
		return l_selectedFeature;
		
		else
			return 404;
	}
	
	
}	
