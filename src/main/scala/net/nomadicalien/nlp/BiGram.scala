package net.nomadicalien.nlp

/**
 * @author Shawn Garner
 * Created: 3/16/13 5:27 PM
 */
object BiGram {
  val preciseBigram = List(
    List(0.0030343d, 0.025581d, 0.0396549d, 0.0453798d, 0.0039724d, 0.0103247d, 0.0198893d, 0.0042167d, 0.0400169d, 0.0029861d, 0.0137632d, 0.0805767d, 0.0288265d, 0.2172862d, 0.0032967d, 0.0222721d, 0.0029257d, 0.1132278d, 0.0961798d, 0.1345468d, 0.0125537d, 0.0334082d, 0.0116307d, 0.003707d, 0.0261209d, 0.0046209d),
    List(0.0582893d, 0.0061565d, 0.0003657d, 0.0025491d, 0.341958d, 0.0004923d, 0.0007138d, 0.0006505d, 0.048195d, 0.007976d, 0.0002866d, 0.1153426d, 0.0015682d, 0.0006347d, 0.0969419d, 0.0003024d, 0.0002866d, 0.0743167d, 0.022975d, 0.0110296d, 0.1198993d, 0.0014416d, 0.0003182d, 0.0002866d, 0.0867368d, 0.0002866d),
    List(0.1235678d, 0.0006108d, 0.0221288d, 0.0011083d, 0.1721753d, 0.0005927d, 0.0005837d, 0.1707553d, 0.0571327d, 0.0005837d, 0.0459803d, 0.0494716d, 0.0005837d, 0.0006199d, 0.1765079d, 0.0005927d, 0.0023022d, 0.0382649d, 0.0016329d, 0.0912593d, 0.0364198d, 0.0005837d, 0.0005837d, 0.0005837d, 0.0047806d, 0.0005927d),
    List(0.0518866d, 0.0243578d, 0.0241106d, 0.0319607d, 0.1463354d, 0.024532d, 0.0293365d, 0.0245601d, 0.1033423d, 0.0242005d, 0.0240432d, 0.0331183d, 0.0259987d, 0.0268977d, 0.0635804d, 0.0240151d, 0.0238746d, 0.0417439d, 0.0545558d, 0.024768d, 0.0417326d, 0.026628d, 0.0252176d, 0.0238521d, 0.0314999d, 0.0238521d),
    List(0.0681473d, 0.0148398d, 0.0359922d, 0.0979034d, 0.0452697d, 0.022124d, 0.0188434d, 0.0153362d, 0.0262786d, 0.0138205d, 0.0146567d, 0.0475575d, 0.0321868d, 0.0950871d, 0.0167934d, 0.0236907d, 0.0157024d, 0.1443117d, 0.0848393d, 0.0377137d, 0.0149889d, 0.0292912d, 0.0206479d, 0.0240739d, 0.0261597d, 0.0137431d),
    List(0.0782012d, 0.0145817d, 0.0146612d, 0.0145519d, 0.1071697d, 0.0643726d, 0.0143632d, 0.0144426d, 0.0964009d, 0.0143433d, 0.0143632d, 0.0410071d, 0.014383d, 0.0144327d, 0.1688619d, 0.0145122d, 0.0143532d, 0.0968777d, 0.0182972d, 0.0485373d, 0.0478419d, 0.0143532d, 0.014542d, 0.0143433d, 0.0158633d, 0.0143433d),
    List(0.0716885d, 0.0128292d, 0.0129655d, 0.0144528d, 0.1606776d, 0.0127177d, 0.0226081d, 0.1544558d, 0.0625541d, 0.0124698d, 0.0124822d, 0.0519572d, 0.015432d, 0.0252356d, 0.0697674d, 0.0130027d, 0.012445d, 0.1009136d, 0.0385592d, 0.0186916d, 0.0380759d, 0.0124822d, 0.012792d, 0.012445d, 0.015717d, 0.0125813d),
    List(0.1623922d, 0.0051427d, 0.006415d, 0.0048875d, 0.4640725d, 0.0048317d, 0.0044049d, 0.0044408d, 0.1296357d, 0.004369d, 0.004373d, 0.0057968d, 0.0056732d, 0.0056612d, 0.0744247d, 0.0044129d, 0.0046881d, 0.0160868d, 0.0065746d, 0.0340943d, 0.0175187d, 0.004369d, 0.005398d, 0.004369d, 0.0115999d, 0.004369d),
    List(0.0188657d, 0.0090805d, 0.0508454d, 0.0385135d, 0.0502325d, 0.0293096d, 0.0329517d, 0.0022436d, 0.0026487d, 0.0021696d, 0.0094786d, 0.0548503d, 0.043459d, 0.2640656d, 0.0519479d, 0.0084359d, 0.0025994d, 0.0459281d, 0.1179643d, 0.1220045d, 0.0028671d, 0.0257449d, 0.0021872d, 0.0040471d, 0.0021661d, 0.0053926d),
    List(0.2108061d, 0.0001423d, 0.0001423d, 0.0001423d, 0.1907843d, 0.0001423d, 0.0001423d, 0.0001423d, 0.0005776d, 0.0001423d, 0.0001423d, 0.0001423d, 0.0001423d, 0.0001423d, 0.2645601d, 0.0001423d, 0.0001423d, 0.0001423d, 0.0001423d, 0.0001423d, 0.3300661d, 0.0001423d, 0.0001423d, 0.0001423d, 0.0003599d, 0.0001423d),
    List(0.0284137d, 0.0126574d, 0.0119961d, 0.0132021d, 0.3436565d, 0.0156531d, 0.0119572d, 0.0122684d, 0.1415868d, 0.0114903d, 0.0118015d, 0.0300866d, 0.0124629d, 0.1124473d, 0.0228504d, 0.0126963d, 0.0114903d, 0.0119182d, 0.0728426d, 0.0137857d, 0.0144859d, 0.0114903d, 0.0156142d, 0.0114903d, 0.020166d, 0.0114903d),
    List(0.1072277d, 0.0060992d, 0.0075936d, 0.0724113d, 0.1712922d, 0.0189501d, 0.0067278d, 0.0057019d, 0.1163366d, 0.0055596d, 0.0109205d, 0.1410657d, 0.0110866d, 0.0065203d, 0.0881976d, 0.0078131d, 0.0055536d, 0.007392d, 0.028563d, 0.0236112d, 0.0199938d, 0.0097107d, 0.0081274d, 0.0055477d, 0.1024242d, 0.0055714d),
    List(0.1600138d, 0.034677d, 0.0062484d, 0.0086215d, 0.2556965d, 0.0078629d, 0.0061026d, 0.0063846d, 0.0938295d, 0.0061026d, 0.0060831d, 0.0076587d, 0.0282677d, 0.0090398d, 0.1159363d, 0.0545955d, 0.0060831d, 0.0230741d, 0.0310785d, 0.0069292d, 0.0446266d, 0.0061123d, 0.0061998d, 0.0060831d, 0.0566088d, 0.0060831d),
    List(0.0338957d, 0.0104282d, 0.0522023d, 0.1866202d, 0.0948016d, 0.0151886d, 0.1287544d, 0.0127649d, 0.0394639d, 0.0111073d, 0.0170203d, 0.0186605d, 0.0105432d, 0.0183923d, 0.0752414d, 0.0102193d, 0.0108566d, 0.0102994d, 0.0525853d, 0.1054854d, 0.0192664d, 0.013235d, 0.0107347d, 0.0102193d, 0.022d, 0.0100138d),
    List(0.013175d, 0.0121498d, 0.0175662d, 0.0251651d, 0.0078014d, 0.1184448d, 0.0109234d, 0.0067102d, 0.0136432d, 0.0050553d, 0.0155487d, 0.0317222d, 0.0629022d, 0.1508413d, 0.03792d, 0.0187234d, 0.0050883d, 0.1224008d, 0.0371255d, 0.0541232d, 0.1385776d, 0.0213376d, 0.0536946d, 0.0053949d, 0.0082498d, 0.0057146d),
    List(0.1308024d, 0.0027943d, 0.0027943d, 0.0028229d, 0.2237058d, 0.0024944d, 0.0024373d, 0.0244742d, 0.056237d, 0.0023802d, 0.0024659d, 0.0981113d, 0.003437d, 0.0029372d, 0.1235758d, 0.0631208d, 0.0023516d, 0.1386003d, 0.0246455d, 0.0432119d, 0.0294442d, 0.0023516d, 0.0035227d, 0.0023516d, 0.006579d, 0.0023516d),
    List(0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0004041d, 0.0004041d, 0.0001757d, 0.9951506d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d, 0.0001757d),
    List(0.0819802d, 0.011836d, 0.0203068d, 0.0370348d, 0.2431808d, 0.0142894d, 0.0193663d, 0.011271d, 0.087871d, 0.0086714d, 0.0173474d, 0.0203542d, 0.0278726d, 0.030824d, 0.1005652d, 0.0134322d, 0.0086594d, 0.0275684d, 0.0545492d, 0.050784d, 0.026d, 0.0156881d, 0.0106152d, 0.0086318d, 0.0370782d, 0.0142223d),
    List(0.0504199d, 0.0160859d, 0.0312214d, 0.0169626d, 0.1334267d, 0.0164976d, 0.0159106d, 0.0724237d, 0.0660471d, 0.0154913d, 0.0208198d, 0.0268915d, 0.0219938d, 0.0176906d, 0.0645682d, 0.0442262d, 0.0162727d, 0.0156324d, 0.0618315d, 0.1423609d, 0.048457d, 0.0155218d, 0.0208274d, 0.0154418d, 0.0175343d, 0.0154418d),
    List(0.0487505d, 0.00958d, 0.0131405d, 0.0094884d, 0.0986644d, 0.0103428d, 0.0094614d, 0.3447138d, 0.0760968d, 0.0094264d, 0.0094372d, 0.0240483d, 0.010332d, 0.0105261d, 0.1007263d, 0.0095019d, 0.0094237d, 0.0404491d, 0.0339588d, 0.0265387d, 0.0279942d, 0.0094237d, 0.0172912d, 0.009421d, 0.0215632d, 0.0096986d),
    List(0.0283857d, 0.0204136d, 0.0482069d, 0.0245612d, 0.0331271d, 0.0081105d, 0.0527911d, 0.0023039d, 0.0320531d, 0.0022427d, 0.0023912d, 0.1088667d, 0.0331009d, 0.1178342d, 0.004251d, 0.0470805d, 0.0024086d, 0.1649248d, 0.1229685d, 0.1272209d, 0.0022689d, 0.0031683d, 0.0024348d, 0.0031159d, 0.0025309d, 0.0032382d),
    List(0.1023026d, 0.0000784d, 0.0000784d, 0.0050343d, 0.6797711d, 0.0000784d, 0.0000784d, 0.0003155d, 0.1468345d, 0.0000784d, 0.0000784d, 0.0002207d, 0.0000784d, 0.0129068d, 0.0429979d, 0.0000784d, 0.0000784d, 0.0009083d, 0.0003867d, 0.0000784d, 0.0026631d, 0.0006712d, 0.0000784d, 0.0000784d, 0.0039672d, 0.0000784d),
    List(0.1878216d, 0.0049006d, 0.0048661d, 0.0064615d, 0.1651301d, 0.0058762d, 0.0046021d, 0.1939049d, 0.1834601d, 0.0045677d, 0.0050727d, 0.0135089d, 0.0048432d, 0.0418475d, 0.0979508d, 0.004648d, 0.0045792d, 0.0127743d, 0.0172162d, 0.0063812d, 0.0057384d, 0.0045677d, 0.0048891d, 0.0045677d, 0.0052564d, 0.0045677d),
    List(0.0638412d, 0.0038268d, 0.161185d, 0.0048318d, 0.0592468d, 0.0038268d, 0.0039704d, 0.0170357d, 0.1161025d, 0.0038268d, 0.0038268d, 0.0052626d, 0.0039704d, 0.0038268d, 0.0094262d, 0.2196201d, 0.0069855d, 0.0038268d, 0.0065547d, 0.2398641d, 0.023353d, 0.0089955d, 0.0039704d, 0.0131592d, 0.0058369d, 0.0038268d),
    List(0.0352509d, 0.0319652d, 0.0292316d, 0.0301069d, 0.0843076d, 0.0295548d, 0.0283428d, 0.0287468d, 0.0421724d, 0.0280466d, 0.02806d, 0.0312245d, 0.0319382d, 0.0302819d, 0.1485809d, 0.0308206d, 0.0280331d, 0.0329213d, 0.0605266d, 0.0389944d, 0.0286256d, 0.0281004d, 0.0296356d, 0.0281678d, 0.0281274d, 0.0282351d),
    List(0.4239743d, 0.00275d, 0.0080185d, 0.0087711d, 0.3058107d, 0.0019974d, 0.0019974d, 0.0024992d, 0.0729959d, 0.0022483d, 0.0019974d, 0.0218168d, 0.0019974d, 0.0019974d, 0.0750029d, 0.0019974d, 0.0019974d, 0.0022483d, 0.0037535d, 0.0024992d, 0.0057606d, 0.0030009d, 0.0019974d, 0.0019974d, 0.0120325d, 0.0288414d)
  )

  val biGramLookup: Map[String, Double] = {
    val listOfEntries = for (
      letter1 <- 'a' to 'z';
      letter2 <- 'a' to 'z'
    ) yield s"$letter1|$letter2" -> probOfAGivenB(letter1, letter2)

    listOfEntries.toMap
  }

  def probOfAGivenB(a : Char, b : Char) : Double = {
    val bRowIndex = b - 'a'
    val aColumnIndex = a - 'a'

    preciseBigram(bRowIndex)(aColumnIndex)
  }
}
