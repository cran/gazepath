\name{gazepath}
\alias{gazepath}
\alias{summary.gazepath}
\alias{plot.gazepath}
\alias{print.gazepath}
\alias{head.gazepath}
\alias{tail.gazepath}
\title{
Transform Raw Eye-Tracking Data into Fixations and Saccades
}
\description{
This function transforms raw eye-tracking data into fixations and saccades using a non-parametric speed-based approach to do this on a trial basis. The method is especially useful when there are large differences in data quality between or within participants. Lower data quality results in more conservative thresholds, thereby controlling for noise levels.
}
\usage{
gazepath(data, x1, y1, x2 = NULL, y2 = NULL, d1, d2 = NULL, trial,
height_px, height_mm, width_px, width_mm, extra_var = NULL,
res_x = 1280, res_y = 1024, samplerate = 500, 
method = "Mould", posthoc = FALSE, thres_vel = 35, 
thres_dur = 100, min_dist = 250, in_thres = 150)

\method{summary}{gazepath}(object, ..., 
complete_only = FALSE, fixations_only = FALSE)
\method{plot}{gazepath}(x, ..., trial_index = 1)
\method{print}{gazepath}(x, ...)
\method{head}{gazepath}(x, ...)
\method{tail}{gazepath}(x, ...)

}

\arguments{
  \item{data}{
The dataframe with at least the raw x- and y-coordinates, the distance to the screen in mm and a trial index.
}
  \item{x1}{
The column name (between quotes, e.g. 'x1') or the number of the column in the dataframe containing the x-coordinates
}
  \item{y1}{
The column name (between quotes, e.g. 'y1') or the number of the column in the dataframe containing the y-coordinates
}
  \item{x2}{
When tracking was binocular, the column name (between quotes, e.g. 'x2') or number of the dataframe containing the x-coordinates of the second eye
}
  \item{y2}{
When tracking was binocular, the column name (between quotes, e.g. 'y2') or number of the dataframe containing the y-coordinates of the second eye
}
  \item{d1}{
The column name (between quotes, e.g. 'd2') or numberof the dataframe containing the distance in mm
}
  \item{d2}{
When tracking was binocular, the column name (between quotes, e.g. 'd2') or number of the dataframe containing the distance in mm of the second eye
}
  \item{trial}{
The column name (between quotes, e.g. 'TRIAL_INDEX') or number of the dataframe containing the trial or stimuli index
}
  \item{height_px}{
The height of the stimuli in pixels, can be a single value or a vector of length number of trials when stimuli differ in size per trial
}
  \item{height_mm}{
The height of the stimuli in mm, can be a single value or a vector of length number of trials when stimuli differ in size per trial
}
  \item{width_px}{
The width of the stimuli in pixels, can be a single value or a vector of length number of trials when stimuli differ in size per trial
}
  \item{width_mm}{
The height of the stimuli in pixels, can be a single value or a vector of length number of trials when stimuli differ in size per trial trials
}
  \item{extra_var}{
A vector of names of the variables that must return in the output file, for example, condition, stimuli name, etc.
}
  \item{res_x}{
The horizontal resolution of the monitor in pixels
}
  \item{res_y}{
The vertical resolution of the monitor in pixels
}
  \item{samplerate}{
The samplerate, e.g., 60 for a 60 Hz scanner, 500 for a 500 Hz scanner
}
  \item{method}{
Must be one of the following methods: Mould, MouldDur, Mould.all, Mould.allDur, gazepath, Tobii, Eyelink.

Mould estimates velocity threshold per trial and duration thresholds per person (recommended for high, > 250 Hz, samplerate and high quality data)

MouldDur estimates velocity threshold per trial and uses the duration thresholds specified under 'thres_dur' (recommended for low, < 250 Hz, samplerate data and data of low quality)

Mould.all estimates velocity threshold and duration threshold for all trials (recommended for high, > 250 Hz, samplerate data and data with short trial times, < 2 seconds)

Mould.allDur estimates one velocity threshold for all trials and uses the duration threshold specified under 'thres_dur' (recommended for high, > 250 Hz, samplerate data and data with short trial times, < 2 seconds and few trials)

gazepath estimates velocity thresholds per person and uses the duration thresholds specified under 'thres_dur'. Furthermore, extra preprocessing steps are performed, such as, last point interpolation of missing data sequences of length in_thres and combining succesive fixations that overlap in space.

Tobii derives fixations and saccades based on the algorithm described in the ClearView 2.7 manual

Eyelink classifies saccades as sequences of samples (> 4ms) that have a velocity over 35 deg/s, acceleration over 8000 deg/s^2 and defliction over 0.1 deg of visual angle. Fixations are the sequences of samples that are not saccades and pass the duration threshold of 100 ms
}
  \item{posthoc}{
TRUE or FALSE (default). When TRUE successive fixations close in space are merged.
}
  \item{thres_vel}{
The desired velocity threshold, will only be used for method 'Eyelink'
}
  \item{thres_dur}{
The desired duration threshold, will be used for methods 'gazepath', 'Eyelink' and 'MouldDur' 
}
  \item{min_dist}{
The minimal distance from the screen in mm, samples below this distance are considered unreliable and are omitted from further analysis.
}
  \item{in_thres}{
The durations threshold in msec used by the gazepath method to interpolate.
}
  \item{object}{
  Object of class 'gazepath' can be used in the plot, summary and print function
  }
  \item{x}{
  Object of class 'gazepath' can be used in the plot, summary, head, tail and print function
  }
  \item{trial_index}{
  Argument of the plot function, refering to the trial to be plotted
  }
  \item{complete_only}{
  Argument (TRUE/FALSE) to indicate whether only fixations that have incoming out outgoing saccades and saccades that are between two fixations should be selected
  }
  \item{fixations_only}{
  Argument (TRUE/FALSE) to indicate whether only fixations should be returned by summary, instead of fixations and saccades
  }
  \item{...}{
  Not used currently
  }
}
\details{
no details
}
\value{
gazepath returns a list of 16 items. names(list) and be used to extract useful information, the plot() and summary() functions can be used to verify the procedure and obtain the fixations.

\item{1. Classifications}{List of length number of trials with a vector indicating whether a sample is classified as fixation 'f', saccade 's', unknown 'u' or NA}
\item{2. x-coordinates}{List of length number of trials with vectors of the x-coordinates of each trial}

\item{3. y-coordinates}{List of length number of trials with vectors of the y-coordinates of each trial}

\item{4. Method}{single value indicating what method was used to classify fixations and saccades}

\item{5. Robustness}{Vector containing the mean durations of usable data segments per trial in milliseconds. Higher values indicate better data quality}

\item{6. Presicion}{Vector containing the mean differences in pixels between raw data points and a smoothed version of the data. Higher values indicate poorer data quality}

\item{7. Velocity thresholds}{Vector with the velocity thresholds of each trial (methods: gazepath, Mould, MouldDur) in deg/s, or a single value when a single velocity threshold is used(methods: eyelink, Mould.all, Tobii)}

\item{8. Duration threshold}{Vector with the duration thresholds of each trial (method: Mould) in milliseconds, or a single value when a single duration threshold is used(methods: gazepath, MouldDur, eyelink, Mould.all, Tobii)}

\item{9. Speed}{List of length number of trials with vectors containing the speed at each sample}

\item{10. Samplerate}{The samplerate}

\item{11. Head target distance}{List of length number of trials with vectors containing the distance to the screen in mm for each sample}

\item{12. Height in pixels}{Vector of length number of trials with the stimuli heights in pixels}

\item{13. Height in mm}{Vector of length number of trials with the stimuli heights in mm}

\item{14. Width in pixels}{Vector of length number of trials with the stimuli widths in pixels}

\item{15. Widtht in mm}{Vector of length number of trials with the stimuli widths in mm}

\item{16. Fixations and saccades per trial}{List of length number of trials with dataframes containing the fixations and saccades, see also summary()}


}
\references{
Mould, M. S., Foster, D. H., Amano, K., & Oakley, J. P. (2012). A simple nonparametric method for classifying eye fixations. Vision research, 57, 18-25.
}
\author{
Daan van Renswoude & Ingmar Visser
}

\examples{
#### Example
data(screen)
data(eye_data)
example <- gazepath(eye_data, x1 = 1, y1 = 2, d1 = 4, trial = 3, 
height_px = screen[,1], height_mm = screen[,2], 
width_px = screen[,3], width_mm = screen[,4], 
method = 'Mould', samplerate = 500)

## Use different i's to check the estimated fixations per trial
i <- 1
plot(example, trial_index = i)

## The estimated fixations, orderded per trial
summary(example, complete_only = FALSE)
}
\keyword{ fixations }
\keyword{ saccades }
