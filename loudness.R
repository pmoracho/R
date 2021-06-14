library(soundgen)

scaleSPL = function(x, scale = NULL, SPL_measured = 70, Pref = 2e-5) {
  tmp <-  (x - mean(x))
  x_refScaled = tmp / Pref  # range(x_refScaled)
  RMS = sqrt(mean(x_refScaled ^ 2))
  SPL_internal = 20 * log10(RMS)  # dB-SPL
  c = 10 ^ ((SPL_measured - SPL_internal) / 20)
  x_scaled = c * x_refScaled  # range(x_scaled)
  # plot(x_scaled[5000:6000], type = 'l')
  # check that the new RMS is SPL_measured:
  # 20 * log10(sqrt(mean(x_scaled^2))) should be ~SPL_measured
  
  # correct according to scale (eg if the original sound is quieter than the max
  # possible amplitude, adjust loudness for that)
  if (is.numeric(scale)) {
    x_scaled = x_scaled / (scale / max(abs(x)))
  }
  return(x_scaled)
}

mi_getLoudness = function(x,
                      samplingRate = NULL,
                      scale = NULL,
                      windowLength = 50,
                      step = NULL,
                      overlap = 50,
                      SPL_measured = 70,
                      Pref = 2e-5,
                      spreadSpectrum = TRUE,
                      plot = TRUE,
                      mar = c(5.1, 4.1, 4.1, 4.1),
                      ...) {
    # import sound
    if (is.null(step)) step = windowLength * (1 - overlap / 100)
    if (class(x)[1] == 'character') {
        extension = substr(x, nchar(x) - 2, nchar(x))
        if (extension == 'wav' | extension == 'WAV') {
            sound_wav = tuneR::readWave(x)
        } else if (extension == 'mp3' | extension == 'MP3') {
            sound_wav = tuneR::readMP3(x)
        } else {
            stop('Input not recognized: must be a numeric vector or wav/mp3 file')
        }
        samplingRate = sound_wav@samp.rate
        sound = sound_wav@left
        scale = 2 ^ (sound_wav@bit - 1) # range(sound)
        rm("sound_wav")
    } else if (class(x)[1] == 'numeric' & length(x) > 1) {
        if (is.null(samplingRate)) {
            stop ('Please specify samplingRate, eg 44100')
        } else {
            sound = x
        }
        m = max(abs(sound))
        if (is.null(scale)) {
            scale = max(m, 1)
            warning(paste('Scale not specified. Assuming that max amplitude is', scale))
        } else if (is.numeric(scale)) {
            if (scale < m) {
                scale = m
                warning(paste('Scale exceeds the observed range; resetting to', m))
            }
        }
    }
    if (samplingRate < 2000) return(NA)  # need at least 8 barks (1 kHz) Niquist
    # scale to dB SPL
    sound_scaled = scaleSPL(sound,
                            scale = scale,
                            SPL_measured = SPL_measured,
                            Pref = Pref)
    return(TRUE)
    # range(sound); range(sound_scaled)
    # log10(sqrt(mean(sound_scaled ^ 2))) * 20
    # (should be the same as SPL_measured w/o scale adjustment)

    # get power spectrum
    powerSpec = spectrogram(
        sound_scaled, samplingRate = samplingRate,
        windowLength = windowLength, step = step,
        output = 'original', normalize = FALSE,
        padWithSilence = FALSE,
        plot = plot, mar = mar, ...) ^ 2
    # range(log10(powerSpec) * 10)

    # normalize power spectrum by the size of STFT frame
    # windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
    powerSpec_scaled = powerSpec / nrow(powerSpec)  # same as * 2 / windowLength_points
    # range(log10(powerSpec_scaled) * 10)
    # image(t(powerSpec_scaled))

    # get auditory spectrum
    audSpec = tuneR::audspec(
        powerSpec_scaled,
        sr = samplingRate,
        fbtype = 'bark')$aspectrum
    # image(t(audSpec))
    # range(log10(audSpec) * 10)
    # plot(audSpec[, 1], type = 'l')
    # plot(log10(audSpec[, 1]) * 10, type = 'l')

    # throw away very high frequencies
    if (samplingRate > 44100) {
        message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                      '(27 KHz) as inaudible to humans'))
        audSpec = audSpec[1:27, ]  # max 27 barks
    }

    # apply spreading function (NB: linear, not dB scale!)
    if (spreadSpectrum) {
        nonZeroCols = which(colSums(audSpec) > 0)
        for (c in nonZeroCols) {
            audSpec[, c] = spreadSpec(audSpec[, c])
        }
        # image(t(audSpec))
        # range(log10(audSpec) * 10)
        # plot(audSpec[, 1], type = 'l')
        # plot(log10(audSpec[, 1]) * 10, type = 'l')
    }

    # convert spectrum to sone
    specSone = matrix(0, nrow = nrow(audSpec), ncol = ncol(audSpec))
    for (i in 1:ncol(specSone)) {
        # spectrum in dB SPL
        y = 10 * log10(audSpec[, i])
        # plot(y, type = 'b')

        # dB SPL to phons (8 barks ~= 1 kHz, reference value for equal loudness curves)
        n_phonCurve = which.min(abs(y[8] - as.numeric(names(phonCurves))))
        # correction curve for frame i
        curve = phonCurves[[n_phonCurve]][1:length(y), ]
        y_phon = y + curve$spl[8] - curve$spl
        # plot(y_phon, type = 'b')

        # ignore frequency bins below hearing threshold
        y_phon[y_phon < curve$hearingThres_dB | y_phon < 0] = 0

        # phons to sone
        specSone[, i] = phon2sone(y_phon)
        # plot(specSone[, i], type = 'b')
    }
    # image(t(specSone))
    loudness = apply(specSone, 2, sum)

    # empirical normalization (see commented-out code below the function)
    loudness = loudness / (5.73 +  6.56 * windowLength ^ .35) /
        (.0357 + .0345 * samplingRate ^ .3113)

    # plotting
    if (plot) {
        # spectrogram(sound, samplingRate = 16000, osc = TRUE)
        op = par(c('mar', 'new')) # save user's original pars
        par(new = TRUE, mar = mar)
        # adjust the timing of loudness to match the actual time stamps
        # in getFrameBank (~the middle of each fft frame)
        X = as.numeric(colnames(powerSpec))
        duration = length(sound) / samplingRate
        plot(x = X,
             y = loudness,
             type = "b",
             xlim = c(0, duration * 1000),
             xaxs = "i", yaxs = "i",
             axes = FALSE, bty = "n",
             xlab = "", ylab = "")
        axis(side = 4, at = pretty(range(loudness)))
        mtext("Loudness, sone", side = 4, line = 3)
        par('mar' = op$mar, 'new' = op$new)  # restore original pars
    }
    invisible(list(specSone = specSone, loudness = loudness))
}


windowLength = 50
overlap = 50
SPL_measured = 70
Pref = 2e-5
spreadSpectrum = TRUE
plot = TRUE
mar = c(5.1, 4.1, 4.1, 4.1)

x <- '~/Descargas/R_MIC_091018-164437.mp3'
x <- sound
mi_getLoudness('~/Descargas/R_MIC_091018-164437.mp3')
