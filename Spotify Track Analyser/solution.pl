% hazar cakir
% 2017400093
% compiling: yes
% complete: yes


% I highlight the main predicates with "--------"s. 
% given predicate to filter features 
features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).

filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0,
            FilteredFeatures = FilteredTail
        )
    ).


% ----------------------------------------------------------------------
% It uses helper predicates called as albumsToTracks and getTrackNames.
% getArtistTracks(+ArtistName, -TrackIds, -TrackNames) 
getArtistTracks(ArtistName, TrackIds, TrackNames) :-
    artist(ArtistName, _,AlbumList),
    albumsToTracks(AlbumList,TrackIds),
    getTrackNames(TrackIds,TrackNames).


% For a given list of albums returns all tracks that are included in albums.
% albumsToTracks(+AlbumList, -TrackIds)
albumsToTracks([],[]):-!.
albumsToTracks([Head|Tail],TrackIds):-
    album(Head,_,_,Tracks),
    albumsToTracks(Tail,Res),
    append(Tracks,Res,TrackIds).

% For a given list of tracks, returns a list of string which are names of these tracks.
% getTrackNames(+Tracks, -TrackNames)
getTrackNames([],[]):-!.
getTrackNames([Head|Tail],TrackNames):-
    track(Head,Name,_,_,_),
    getTrackNames(Tail,RestOfNames),
    append([Name],RestOfNames,TrackNames).  

% For a given list of albums, returns a list of string which are names of these albums.
% getAlbumNames(+Albums, -AlbumNames)
getAlbumNames([],[]):-!.
getAlbumNames([Head|Tail],AlbumNames):-
    album(Head,Name,_,_),
    getAlbumNames(Tail,RestOfNames),
    append([Name],RestOfNames,AlbumNames).


% ----------------------------------------------------------------------
% It uses helper predicate sumFeature.
% It sums all features and than divides into element number to find average value.
% albumFeatures(+AlbumId, -AlbumFeatures)
albumFeatures(AlbumId, AlbumFeatures):-
    album(AlbumId,_,_,Tracks),
    sumFeature(Tracks,Result,0,Total),
    findall(X,
    (        
         member(Y, Result),X is Y/Total    
    ),
    AlbumFeatures).


% TrackList is the list of the track ids'. Result is the list of features which are sum of all tracks.
% Counter is to count how many tracks are there and Total is the number of the tracks.
% It uses getFeature and sumTwoList predicates to get a track's features and sum two feature as lists.
% sumFeature(+TrackList, -Result, + Counter , -Total)
sumFeature([],[],Number,Number):-!.
sumFeature([Head|Tail],Result,Number,Total):-
    NewNum is Number + 1,
    sumFeature(Tail,TempRes, NewNum,Total),
    getFeature(Head,HeadFeature),
    sumTwoList(HeadFeature,TempRes,Result).


% Sums two given list. If one of them is an empty list, returns other one as result.
% sumTwoList(+List1, +List2, Result)
sumTwoList([],[],[]):-!.
sumTwoList(A,[],A):-!.
sumTwoList([],A,A):-!.
sumTwoList([H1|T1],[H2|T2],[S|R]):-
    S is H1 + H2,
    sumTwoList(T1,T2,R).


% For a given Track id, returns its features as a list.
% getFeature(+TrackId,-FeatureList)
getFeature(TrackId,FeatureList):-
    track(TrackId,_,_,_,FeatureTemp),
    filter_features(FeatureTemp, FeatureList).

% ----------------------------------------------------------------------
% It uses helper predicate sumFeature and getArtistTracks.
% It sums all features and than divides into element number to find average value.
% artistFeatures(+ArtistName, -ArtistFeatures) 
artistFeatures(ArtistName, ArtistFeatures):-
    getArtistTracks(ArtistName,Tracks,_),
    sumFeature(Tracks,Result,0,Total),
    findall(X,
    (        
         member(Y, Result),X is Y/Total    
    ),
    ArtistFeatures).


% ----------------------------------------------------------------------
% It uses getFeature and distance predicates as helper.
% Finds the distance between two track. "distance" predicate is calculates distance between two feature list.
% trackDistance(+TrackId1, +TrackId2, -Score)
trackDistance(TrackId1,TrackId2,Score):-
    getFeature(TrackId1,Feature1), getFeature(TrackId2,Feature2),
    distance(Feature1,Feature2,Score).

% It uses sumSquare and sqrt predicates as helper.
% Calculates distance between two feature list as wanted.
% distance(+List1, +List2, -Score)
distance(List1,List2,Score):-
    sumSquare(List1,List2,Total),
    Score is sqrt(Total).

% Evaluates the sum part of the distance.
% sumSquare(+List1, +List2, -Score)
sumSquare([],[],0):-!.
sumSquare([H1|T1],[H2|T2],Score):-
    sumSquare(T1,T2,S),
    Score is (H1-H2)*(H1-H2) + S.

% ----------------------------------------------------------------------
% With the help of the previous predicates ( such as albumFeatures and distance), calculates distance between two albums.
% albumDistance(+AlbumId1, +AlbumId2, -Score)
albumDistance(AlbumId1, AlbumId2, Score):-
    albumFeatures(AlbumId1,F1), albumFeatures(AlbumId2,F2),
    distance(F1,F2,Score).


% ----------------------------------------------------------------------
% With the help of the previous predicates ( such as artistFeatures and distance), calculates distance between two artists.
% artistDistance(+ArtistName1, +ArtistName2, -Score)
artistDistance(ArtistName1, ArtistName2, Score):-
    artistFeatures(ArtistName1,F1),artistFeatures(ArtistName2,F2),
    distance(F1,F2,Score).


% ----------------------------------------------------------------------
% It finds most similar 30 tracks to base track.
% First it makes a List of distance-TrackId pair in findall.
% Then it sorts the list with keysort predicate.
% After then, it chooses the first 30 element with shorten predicate.
% With pairToList predicate, it gets the most similar 30 trackIds.
% With getTrackNames, it acquires the track names.
% findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 
findMostSimilarTracks(TrackId, SimilarIds, SimilarNames):-
    findall(Y-X,
    (        
         track(X,_,_,_,_),\+(X = TrackId), trackDistance(TrackId,X,Y)   
    ),
    List),
    keysort(List,Similar),
    shorten(Similar,Similar30,0),
    pairToList(Similar30,_,SimilarIds),
    getTrackNames(SimilarIds,SimilarNames).

% For a given LongList, takes its first 30 elements.
% shorten(+LongList, -ShortList, +Counter)
shorten([],[],_).
shorten(_,[],30).
shorten([H|T],[H|ResTemp],Counter):-  
    NewCounter is Counter + 1,
    shorten(T,ResTemp,NewCounter).
    
% Takes a list of pairs and returns two distinc list.
% pairToList(+PairList, -List1, -List2)
pairToList([],[],[]):-!.
pairToList([X-Y|Tail],[X|L1rest],[Y|L2rest]):- pairToList(Tail,L1rest,L2rest).


% ----------------------------------------------------------------------
% It finds most similar 30 albums to base album.
% First it makes a List of distance-AlbumId pair in findall.
% Then it sorts the list with keysort predicate.
% After then, it chooses the first 30 element with shorten predicate.
% With pairToList predicate, it gets the most similar 30 albumIds.
% With getAlbumNames, it acquires the album names.
% findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 
findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames):-
    findall(Y-X,
    (        
         album(X,_,_,_),\+(X = AlbumId), albumDistance(AlbumId,X,Y)   
    ),
    List),
    keysort(List,Similar),
    shorten(Similar,Similar30,0),
    pairToList(Similar30,_,SimilarIds),
    getAlbumNames(SimilarIds,SimilarNames).


% ----------------------------------------------------------------------
% It finds most similar 30 artists to base artist.
% First it makes a List of distance-artistName pair in findall.
% Then it sorts the list with keysort predicate.
% After then, it chooses the first 30 element with shorten predicate.
% With pairToList predicate, it gets the most similar 30 artistNames.
% findMostSimilarArtists(+ArtistName, -SimilarArtists)
findMostSimilarArtists(ArtistName, SimilarArtists):-
    findall(Y-X,
    (        
         artist(X,_,_),\+(X = ArtistName), artistDistance(ArtistName,X,Y)   
    ),
    List),
    keysort(List,Similar),
    shorten(Similar,Similar30,0),
    pairToList(Similar30,_,SimilarArtists).



% ----------------------------------------------------------------------
% Filter's the explicit tracks with the help of filter predicate
% filterExplicitTracks(+TrackList, -FilteredTracks) 
filterExplicitTracks(TrackList, FilteredTracks):-
    filter(TrackList,FilteredTracks).

%filter(+TrackList, -FilteredTracks)
filter([],[]):-!.
filter([H1|T1],Result):-
    filter(T1,R),
    track(H1,_,_,_,[Expl|_]),
    (
        (Expl = 0, append([H1],R,Result)) ;
        (Expl = 1, Result = R)
    ).
    
% It uses allGenres predicate, returns a track's all genres.
% getTrackGenre(+TrackId, -Genres) 
getTrackGenre(TrackId, Genres):-
    track(TrackId,_,Artists,_,_),
    allGenres(Artists,GenresDuplicated),
    list_to_set(GenresDuplicated,Genres).

% returns all genres relevant to the track with dublications.
% allGenres(+ListOfArtists, -Genres)
allGenres([],[]):-!.
allGenres([Head|Tail],Genres):-
    allGenres(Tail,SubGenres),
    artist(Head,Genre,_),
    append(Genre,SubGenres,Genres).

% ----------------------------------------------------------------------
% Uses isInclude and getTrackNamesAndArtists predicates additionaly previous ones.
% First finds all tracks that satisfies genre criterion in findall.
% Then it clasify infomation about playlist.
% Creates playlist which satisfies given criteria.
% discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist)
discoverPlaylist(LikedGenres, DislikedGenres, Features, FileName, Playlist):-
    findall(Dist-Id,
    (        
         track(Id,_,_,_,F), 
         getTrackGenre(Id,Genres),
         isInclude(LikedGenres,Genres,Val1), Val1 == 1,
         isInclude(DislikedGenres,Genres,Val2), Val2 == 0,
         filter_features(F, Filtered),
         distance(Features,Filtered,Dist)   
    ),
    List),

    keysort(List,Sorted),
    shorten(Sorted,Sorted30,0),
    pairToList(Sorted30,Distances30,Playlist),
    getTrackNamesAndArtists(Playlist,Names30,Artists30),
    open(FileName, write, Stream), writeln(Stream, Playlist),
    writeln(Stream, Names30), 
    writeln(Stream, Artists30),
    writeln(Stream, Distances30),
    close(Stream).


% For a given list of tracks, returns two lists which are names and artists of these tracks.
% getTrackNamesAndArtists(+TrackIds, -TrackNames, -TrackArtists)
getTrackNamesAndArtists([],[],[]):-!.
getTrackNamesAndArtists([Head|Tail],[Name|RestOfNames],[Artists|RestOfArtists]):-
    track(Head,Name,Artists,_,_),
    getTrackNamesAndArtists(Tail,RestOfNames,RestOfArtists).



% It takes two lists of string and returns Find value as 0 or 1.
% If any element of List1 is substring of at least one element of List2, returns 1, else 0.
% I construct some sort of if-else statement, so that the predicate doesn't search for all lists.
% When it finds result 1 at some point, it returns with no further investigation.
% isInclude(+List1, +List2, -Find)
isInclude([],_,0).
isInclude(_,[],0).
isInclude([GenreHead|GenreTail],[LikedGenreHead|LikedGenreTail],Find):-
    (
        (
            sub_atom(LikedGenreHead,_,_,_,GenreHead),SubFind = 1
        );
        (
            \+(sub_atom(LikedGenreHead,_,_,_,GenreHead)),
            isInclude([GenreHead|GenreTail],LikedGenreTail,SubFind) 
        )
    ),
    (
        (
            (SubFind is 0),
            isInclude(GenreTail,[LikedGenreHead|LikedGenreTail],NewFind),
            Find = NewFind
        );
        (
            SubFind is 1,Find = 1
        )
    ).