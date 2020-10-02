# Wikipedia fetch URLs
# Allows you to pull the MediaWiki source and lightly-processed text for
# Wikipedias.
# https://stackoverflow.com/questions/1625162/get-text-content-from-mediawiki-page-via-api

# Default: wiki://article will pull MediaWiki source from English wikipedia.
defresource wiki => read => q{resource_read "enws://$_[1]"};

# Generate a separate URL scheme for each wikipedia instance, fetched from here
# on 2020 Oct 02: https://en.wikipedia.org/wiki/List_of_Wikipedias
#
# ni enws://List_of_Wikipedias p'/\{\{WP7.?\|([^|]+)/g' gup'join" ", rw{1}'

use constant wiki_source => gen
  q{resource_read "https://%wp.wikipedia.org/wiki/$_[1]?action=raw"};

use constant wiki_text => gen
  q{sni resource_append_op("https://%wp.wikipedia.org/w/api.php?format=json"
                           . "&action=query&prop=extracts&explaintext"
                           . "&titles=$_[1]"),
        perl_mapper_op('my @ps = values %{jd(a)->{query}{pages}};
                        map $_->{extract}, @ps')};

for my $wp (qw/
  aa ab ace ady af ak als am an ang ar arc ary arz as ast atj av avk awa ay az
  azb ba ban bar bat-smg bcl be be-tarask bg bh bi bjn bm bn bo bpy br bs bug
  bxr ca cbk-zam cdo ce ceb ch cho chr chy ckb co cr crh cs csb cu cv cy da de
  din diq dsb dty dv dz ee el eml en eo es et eu ext fa ff fi fiu-vro fj fo fr
  frp frr fur fy ga gag gan gcr gd gl glk gn gom gor got gu gv ha hak haw he hi
  hif ho hr hsb ht hu hy hyw hz ia id ie ig ii ik ilo inh io is it iu ja jam jbo
  jv ka kaa kab kbd kbp kg ki kj kk kl km kn ko koi kr krc ks ksh ku kv kw ky la
  lad lb lbe lez lfn lg li lij lld lmo ln lo lrc lt ltg lv mai map-bms mdf mg mh
  mhr mi min mk ml mn mnw mr mrj ms mt mus mwl my myv mzn na nah nap nds nds-nl
  ne new ng nl nn no nov nqo nrm nso nv ny oc olo om or os pa pag pam pap pcd
  pdc pfl pi pih pl pms pnb pnt ps pt qu rm rmy rn ro roa-rup roa-tara ru rue rw
  sa sah sat sc scn sco sd se sg sh shn si simple sk sl sm sn so sq sr srn ss st
  stq su sv sw szl szy ta tcy te tet tg th ti tk tl tn to tpi tr ts tt tum tw ty
  tyv udm ug uk ur uz ve vec vep vi vls vo wa war wo wuu xal xh xmf yi yo za zea
  zh zh-classical zh-min-nan zh-yue zu /)
{
  # ${wp}ws: Wikipedia Source
  defresource "${wp}ws", read => wiki_source->(wp => $wp);

  # ${wp}wt: Wikipedia Text
  defresource "${wp}wt", read => wiki_source->(wp => $wp);
}
